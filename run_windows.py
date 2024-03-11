import argparse
import csv
import subprocess
import sys
import time
import json
import timeit
import os

if __name__ == "__main__":

    # Create the parser
    parser = argparse.ArgumentParser(description='A command line interface to measure '
                                            +'energy consumption of process level using '
                                            +'scaphander as a meter agent for Windows',
                                    epilog='GitHub repository: https://github.com/joegharbi/rosetta')
    
    # Add the arguments
    parser.add_argument('module',metavar= 'module', type=str,help='Provide the module.')
    parser.add_argument('function', metavar='function', type=str,help='Provide the function.')
    parser.add_argument('parameters', nargs='+', metavar='parameters', type=str,help='Provide one or more arguments as input for the function seperated by space.')
    parser.add_argument('-c','--cmd', metavar='command', default='', type=str,help='Provide the command to run the program executable.')
    parser.add_argument('-e','--exe', metavar='executable', default='erl', type=str,help='Provide the process name of the program you want to measure without extension.')
    parser.add_argument('-r','--rep', metavar='repetition', default=1, type=int,help='Provide the number of repetition.')
    parser.add_argument('-f','--file', metavar='file', default='', type=str,help='Provide the csv file name of the result by default executable_module.')
    
    # Parse the arguments
    args = parser.parse_args()
    
    module = args.module
    function = args.function
    parameters = args.parameters
    rep = args.rep
    result_file = args.file

    # Programming language we are measuring in process level name without extension
    # by default it is erl for Erlang
    prog_lang = args.exe

    measure_cmd = args.cmd

    exe_prog_lang = f"{prog_lang}.exe"


    for parameter in parameters:
        # Repeat the measurement for the given number of repetitions
        for _ in range(rep):  
            # Json file name
            file_name = f"{module}_{function}_{parameter}"

            # Prepare the scaphandre command
            command = "scaphandre json -n 100000 -m 100 -f "+file_name+".json"
            # Start the scaphandre measuring agent
            subprocess.Popen(command,stdout=subprocess.DEVNULL, stderr=subprocess.STDOUT, shell= True)
            # Wait for 5 seconds for scaphandre to create json file to dump the data
            time.sleep(5)

            if (measure_cmd == ''):
                # Create the command function to measure
                erl_command = "erl -noshell -run " + module + " " + function + " " + parameter + " -s init stop"
            print(erl_command)
            process = subprocess.Popen(erl_command,stdout=subprocess.DEVNULL, stderr=subprocess.STDOUT, shell= True)
            start_time = timeit.default_timer()

            # Wait for the function to run
            process.wait()

            end_time = timeit.default_timer()

            # Get the total runtime of the function
            runtime = end_time - start_time

            # Then kill scaphandre process to stop the measurement
            subprocess.run(f'taskkill /F /IM scaphandre.exe', shell=True)
            # Then kill the erlang process
            subprocess.run(f'taskkill /F /IM erl.exe', shell=True)

            current_path = os.getcwd()
            json_file_path = os.path.join(current_path, f"{file_name}.json")

            # Read JSON data from the file
            with open(json_file_path, "r") as file:
                data = json.load(file)

            # Initialize total consumption variables
            total_server_consumption = 0.0
            number_samples = 0
            average_energy = 0

            # Iterate through entries
            for entry in data:
                consumers = entry.get("consumers", [])
                for consumer in consumers:
                    exe = consumer.get("exe", "")
                    consumption = consumer.get("consumption", 0.0)
                    
                    # Check for consumption
                    if exe_prog_lang in exe.lower():
                        total_server_consumption += consumption
                        number_samples +=1
            if (number_samples != 0):
                average_energy = total_server_consumption / number_samples

            final_consumption = average_energy * runtime

            # Check result csv file name
            if(result_file==''):
                result_file = f"{prog_lang}_{module}"
            # Write results to the csv file
            with open(f"{result_file}.csv", 'a', newline='') as csv_file:
                csv_writer = csv.writer(csv_file, delimiter=';')
                csv_writer.writerow([module, function, parameter, number_samples, final_consumption, runtime])

    # Exit the program
    sys.exit()
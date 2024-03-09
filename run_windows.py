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
    parser = argparse.ArgumentParser()
    
    # Add the arguments
    parser.add_argument('erl_module', type=str)
    parser.add_argument('erl_function', type=str)
    parser.add_argument('erl_parameter', type=str)
    parser.add_argument('--exe', default='erl', type=str)
    
    # Parse the arguments
    args = parser.parse_args()
    
    erl_module = args.erl_module
    erl_function = args.erl_function
    erl_parameter = args.erl_parameter

    # Programming language we are measuring in process level name without extention
    # by default it is erl for Erlang
    prog_lang = args.exe

    exe_prog_lang = f"{prog_lang}.exe"

    # Json file name
    file_name = f"{erl_module}_{erl_function}_{erl_parameter}"

    # Prepare the scaphandre command
    command = "scaphandre json -n 100000 -m 100 -f "+file_name+".json"
    # Start the scaphandre measuring agent
    subprocess.Popen(command,stdout=subprocess.DEVNULL, stderr=subprocess.STDOUT, shell= True)
    # Wait for 5 seconds for scaphandre to create json file to dump the data
    time.sleep(5)

    # Create the command function to measure
    erl_command = "erl -noshell -run " + erl_module + " " + erl_function + " " + erl_parameter + " -s init stop"
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

    # Write runtime and function name to the csv file
    with open(f"{prog_lang}_{erl_module}.csv", 'a', newline='') as csv_file:
        csv_writer = csv.writer(csv_file, delimiter=';')
        csv_writer.writerow([erl_module, erl_function, erl_parameter, number_samples, final_consumption, runtime])

    # Exit the program
    sys.exit()
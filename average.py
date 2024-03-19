import pandas as pd
import sys
import os

# Get the input and output file names from the command line arguments
input_file = sys.argv[1]
output_file = sys.argv[2]

# Load the data
df = pd.read_csv(input_file, delimiter=';')

# Add a new column that is the result of dividing the 5th column by the 6th column
df['new_column'] = df.iloc[:, 4] / df.iloc[:, 5]

# Group by the first three columns and calculate the mean of the remaining columns
grouped_df = df.groupby(df.columns.tolist()[:3]).mean().reset_index()

# Round the numbers to 2 decimal places
grouped_df = grouped_df.round(2)

# Check if the file exists and if it's empty
file_exists = os.path.isfile(output_file)
file_is_empty = file_exists and os.stat(output_file).st_size == 0

# Open the file in append mode
with open(output_file, 'a') as f:
    # If the file is empty, write the header
    if file_is_empty:
        f.write(';'.join(grouped_df.columns) + '\n')

    # Write the data
    for _, row in grouped_df.iterrows():
        f.write(';'.join(row.astype(str).values) + '\n')

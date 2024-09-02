#!/bin/bash
# Check if an argument is provided
if [ $# -eq 0 ]; then
    echo "Usage: $0 <output_csv_file>"
    exit 1
fi

# Output CSV file from the argument
output_file="$1"

# Create CSV header
echo "filename,n_simplicies,time" > "$output_file"

# Process each txt file in the folder
for file in tests/*.txt; do
    # Get the filename
    filename=$(basename "$file")
    
    # Count the number of lines
    line_count=$(wc -l < "$file")
    
    # Run the betti command and measure execution time
    execution_time=$(./betti "$file" --time)
    
    # Append results to CSV
    echo "$filename,$line_count,$execution_time" >> "$output_file"
    
    echo "Processed $filename"
done

echo "Processing complete. Results stored in $output_file"

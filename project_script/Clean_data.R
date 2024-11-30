# Source the script containing the extract_first_time_data function
source('C:/Users/Margo/OneDrive/Documents/Fall 2024 Courses/Spatial Data Science Course/final-project-embyrne0/project_script/data_extraction.R')

# Function to flatten the extracted data
flatten_extracted_data <- function(extracted_data) {
  # Initialize a list to store the flattened data
  flattened_data <- list()

  # Loop through each file in the extracted data
  for (file_name in names(extracted_data)) {
    # Extract the variable data (which is currently in 1D vector form)
    var_data <- extracted_data[[file_name]]

    # Check if the data is already flattened (if it is a 1D vector)
    if (is.vector(var_data)) {
      # The data is already flattened, so we just store it
      flattened_data[[file_name]] <- var_data
    } else {
      # If the data is not flattened, flatten it
      flattened_data[[file_name]] <- as.vector(var_data)
    }
  }

  # Return the flattened data
  return(flattened_data)
}

# Example usage:
# Assuming 'extracted_data' contains the data from the previous function call
# Flatten the extracted data
flattened_data <- flatten_extracted_data(extracted_data)

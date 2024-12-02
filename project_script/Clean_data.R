# Source the script containing the extract_first_time_data function
source('C:/Users/Margo/OneDrive/Documents/Fall 2024 Courses/Spatial Data Science Course/final-project-embyrne0/project_script/data_extraction.R')

flatten_extracted_data <- function(first_time_data_2015) {
  # Initialize a list to store the flattened data
  flattened_data <- list()

  # List to hold the 1D vectors
  all_data_list <- list()

  # Loop through each file in the extracted data
  for (file_name in names(first_time_data_2015)) {
    # Extract the variable data (which is currently in a matrix or array)
    var_data <- first_time_data_2015[[file_name]]

    # Print the structure to understand if it's already 1D or needs flattening
    print(paste("Processing file:", file_name))
    print(dim(var_data))  # Check the dimensions of the data

    # Flatten the data if it's not already a 1D vector
    if (length(dim(var_data)) > 1) {
      flattened_data[[file_name]] <- as.vector(var_data)  # Flatten to 1D
    } else {
      flattened_data[[file_name]] <- var_data  # If already 1D, keep as is
    }

    # Convert zeros to NaN
    flattened_data[[file_name]][flattened_data[[file_name]] == 0] <- NaN

    # Add to the list of all flattened data
    all_data_list[[file_name]] <- flattened_data[[file_name]]
  }

  # Convert the list to a data frame or matrix
  all_data_matrix <- do.call(cbind, all_data_list)

  # NaN positions: If one column has NaN, set the corresponding
  # index in all other columns to NaN
    # get rid of NaN across all the rows, to ensure they remain the same size per model group
  for (i in 1:ncol(all_data_matrix)) {
    nan_indices <- which(is.nan(all_data_matrix[, i]))  # Find NaNs in column i
    for (j in 1:ncol(all_data_matrix)) {
      all_data_matrix[nan_indices, j] <- NaN  # Set NaN in the same positions across all columns
    }
  }

  # Drop rows where any NaN exists across the models (columns)
  valid_rows <- complete.cases(all_data_matrix)  # Returns a logical vector (TRUE/FALSE) where NaN's exist
  all_data_matrix <- all_data_matrix[valid_rows, ]  # Keep only valid rows

  # Convert back to list of 1D vectors after dropping NaN rows
  for (i in 1:ncol(all_data_matrix)) {
    flattened_data[[names(all_data_list)[i]]] <- all_data_matrix[, i]
  }

  # Return the cleaned, flattened data
  return(flattened_data)
}

# Example usage:
flattened_cleaned_data <- flatten_extracted_data(first_time_data_2015)

# Check the result to ensure it's correctly flattened and the NaN and zeros are dropped
  # print(flattened_data) option



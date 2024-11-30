# Load necessary libraries

# Source the external file containing the previous code for data loading
source('C:/Users/Margo/OneDrive/Documents/Fall 2024 Courses/Spatial Data Science Course/final-project-embyrne0/project_script/file_script.R')

extract_first_time_data <- function(file_list, var, year = year) {
  # Initialize an empty list to store extracted data
  extracted_data <- list()

  # Loop through the file list to extract the first time step data
  for (file_name in file_list) {
    # Open the NetCDF file
    nc_data <- nc_open(file_name)

    # Extract the time variable (assuming it's named 'time')
    time_var <- ncvar_get(nc_data, "time")

    # Extract the time units and reference date (if available)
    time_units <- ncatt_get(nc_data, "time", "units")$value
    reference_date <- sub("days since ", "", time_units)  # Extract the reference date from the units string
    start_date <- as.Date(reference_date)  # Convert the reference date to Date format

    # Convert the time from days to Date (assuming time is in days since the reference date)
    time_dates <- start_date + time_var

    # Extract the year from the time variable
    time_years <- as.numeric(format(time_dates, "%Y"))

    # Check if the first time step corresponds to the year 2015
    if (time_years[1] != year) {
      message(paste("The first time step in file", file_name, "is not 2015. First year:", time_years[1]))
      next
    }

    # Extract the dimensions of the variable (e.g., 337 x 557 x 86)
    var_dims <- ncvar_get(nc_data, var)

    # Extract the first time step for the entire spatial grid (t=1)
    var_data <- ncvar_get(nc_data, var, start = c(1, 1, 1), count = c(337, 557, 1))

    # Store the extracted data in the list, using the file name as the key
    extracted_data[[file_name]] <- var_data

    # Close the NetCDF file after extraction
    nc_close(nc_data)
  }

  # Return the extracted data
  return(extracted_data)
}

# Example usage
# Assuming 'filtered_files' contains the list of NetCDF files
first_time_data_2015 <- extract_first_time_data(filtered_files, "acabf", year = year)

# Check the extracted data
print(first_time_data_2015)

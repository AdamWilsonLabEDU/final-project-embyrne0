# This is for setting up the project
# Setting the variables (acabf) from expo05 and exp07
# acabf: SurfaceMass balance (SMB)
# Exp05:RCP 8.5
# Exp07: RCP 2.6
# Time = 2015, this is the starting period for the projection

# Download all PAckages
library(ncdf4)  # For reading NetCDF files
library(raster) # package for raster manipulation
library(terra)  # geospatial data analysis
library(ggplot2) # package for plotting
# install.packages('plotly')
library(plotly)

# Create the variables and filtered needed
path = "..../Spatial Data Science Course/final-project-embyrne0/data/acabf_GIS_model_experiments/"

# Define the variable 'acabf'
var <- 'acabf'

# Define the experiment types (Exp05 and Exp07)
exp <- c('exp05', "exp07")

year <- 2015

# Next is to read the files following the var and exp
# Step 1: List all files in the directory that match the experiment types (Exp05 and Exp07)
files <- list.files(path, pattern = paste(exp, collapse = "|"), full.names = TRUE)

# Step 2: Filter files that contain the variable 'acabf'
filtered_files <- files[grep(var, files)]

# Print the filtered files to check
print(filtered_files)

## example of opening one of the files
# nc_data <- nc_open('data/acabf_GIS_VUB_GISM_exp05.nc')
# t <- ncvar_get(nc_data, "time")
# variable <- ncvar_get(nc_data, var)

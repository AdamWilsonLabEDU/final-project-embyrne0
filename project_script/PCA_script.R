# This is for the PCA test

source('C:/Users/Margo/OneDrive/Documents/Fall 2024 Courses/Spatial Data Science Course/final-project-embyrne0/project_script/Clean_data.R')

# change the structure of the cleaned data before running the PCA
  # Shortens the names as they orginally showed the full path name
change_names <- function(data_list) {
  new_names <- sapply(names(data_list), function(x) {
    base_name <- basename(x)  # Extract the base name
    no_ext_name <- tools::file_path_sans_ext(base_name)  # Remove the file extension
    return(no_ext_name)
  })
  names(data_list) <- new_names  # Assign new names to the list
  return(data_list)
}

# Apply the function to your flattened_cleaned_data
flattened_cleaned_data <- change_names(flattened_cleaned_data)


# Convert flattened data into a matrix
data_matrix <- do.call(cbind, flattened_cleaned_data)

# Extract model group names from the column names of the data matrix
model_group_names <- colnames(data_matrix)


# Perform PCA with 2 components
  # Two were choosen as there is only one variable with two experiments
pca_result <- prcomp(data_matrix, center = TRUE, scale. = TRUE, rank. = 2)

# Summarize the PCA result
print(summary(pca_result))

# The principal components (PCs)
pca_result$rotation  # Eigenvectors (loadings)

# The transformed data (scores on the principal components)
pca_scores <- pca_result$x

# Plot PCA scores
pca_plot <- ggplot(pca_scores, aes(x = PC1, y = PC2)) +
  geom_point() +
  labs(title = "PCA: First vs Second Principal Component", x = "PC1", y = "PC2") +
  theme_minimal()

# Display the PCA plot
print(pca_plot)

# Scree plot to see the variance explained by each principal component
scree_plot <- ggplot(data.frame(Variance = pca_result$sdev^2 / sum(pca_result$sdev^2)), aes(x = 1:length(pca_result$sdev), y = Variance)) +
  geom_bar(stat = "identity") +
  labs(title = "Scree Plot", x = "Principal Component", y = "Variance Explained")

# Display the scree plot
print(scree_plot)

# Print loadings for variables based on the experiment type
print("Loadings for variables based on the experiment type:")
print(pca_result$rotation)

# Extract and print the centered and scaled data
centered_data <- pca_result$center
scaled_data <- pca_result$scale

# Print centered data with model names
print("Centered Data:")
print(centered_data)

# Print scaled data with model names
print("Scaled Data:")
print(scaled_data)

# Convert centered and scaled data to data frames
centered_df <- data.frame(ModelGroup = names(centered_data), Center = centered_data)
scaled_df <- data.frame(ModelGroup = names(scaled_data), Scale = scaled_data)

# Plot centered data
centered_plot <- ggplot(centered_df, aes(x = ModelGroup, y = Center)) +
  geom_bar(stat = "identity") +
  labs(title = "Centered Data", x = "Model Group", y = "Center Value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Plot scaled data
scaled_plot <- ggplot(scaled_df, aes(x = ModelGroup, y = Scale)) +
  geom_bar(stat = "identity") +
  labs(title = "Scaled Data", x = "Model Group", y = "Scale Value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Display the centered and scaled data plots
print(centered_plot)
print(scaled_plot)




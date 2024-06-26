# Importing necessary libraries for the project
library(tidyverse)  # For data manipulation and visualization
library(psych)      # For descriptive statistics
library(ggplot2)    # For data visualization
library(corrplot)   # For correlation plot
library(ggcorrplot) # For enhanced correlation plot
library(cluster)    # For clustering

# Function to load and inspect data
load_and_inspect_data <- function(file_path) {
  data <- read.csv(file_path)
  print("Data Summary:")
  print(summary(data))
  print("Data Structure:")
  print(str(data))
  print("Descriptive Statistics:")
  print(describe(data))
  print("Data Head:")
  print(head(data))
  missing_values <- colSums(is.na(data))
  print("Missing Values:")
  print(missing_values)
  return(data)
}

# Function to clean data
clean_data <- function(data) {
  data <- na.omit(data)
  print("Cleaned Data Summary:")
  print(summary(data))
  return(data)
}

# Function to transform data
transform_data <- function(data) {
  names(data) <- c('ID', 'Gender', 'Age', 'Income', 'Spending_Score')
  data$Gender <- ifelse(data$Gender == 'Male', 1, ifelse(data$Gender == 'Female', 0, NA))
  data$Rating <- cut(data$Spending_Score, breaks = c(0, 35, 70, 100), labels = c("Bad", "Normal", "Good"))
  data$Rating <- as.factor(data$Rating)
  data$Gender <- as.factor(data$Gender)  # Convert Gender to factor for plotting
  levels(data$Gender) <- c("Female", "Male")  # Set levels to meaningful names
  print("Transformed Data Head:")
  print(head(data))
  return(data)
}

# Function for correlation analysis
correlation_analysis <- function(data) {
  cor_matrix <- cor(data %>% select(Age, Income, Spending_Score))
  print("Correlation Matrix:")
  print(cor_matrix)
  corrplot(cor_matrix, method = 'circle')
}

# Function to create histograms
create_histograms <- function(data) {
  cols <- c('Age', 'Income', 'Spending_Score')
  for (i in cols) {
    print(ggplot(data, aes_string(x = i)) + 
            geom_histogram(fill = 'darkblue', binwidth = 2) + 
            xlab(i) + theme_bw() + ggtitle(paste0(i, ' Histogram')))
  }
}

# Function to create boxplots
create_boxplots <- function(data, by_var) {
  cols <- c('Age', 'Income', 'Spending_Score')
  for (i in cols) {
    print(ggplot(data, aes_string(x = by_var, y = i, fill = by_var)) + 
            geom_boxplot() + ylab(i) + theme_bw() + ggtitle(paste0(i, ' Boxplot by ', by_var)))
  }
}

# Function to calculate and plot averages
calculate_and_plot_averages <- function(data, group_var, value_var, fill_color, plot_title) {
  avg_data <- data %>% group_by_at(group_var) %>% summarize(N = n(), avg_value = mean(get(value_var), na.rm = TRUE))
  print(paste0("Average ", value_var, " by ", group_var, ":"))
  print(avg_data)
  ggplot(avg_data, aes_string(x = group_var, y = 'avg_value')) + 
    geom_col(fill = fill_color) + theme_bw() + ggtitle(plot_title)
}

# Function to create scatter plots
create_scatter_plots <- function(data, x_var, y_var, color_var, plot_title) {
  ggplot(data, aes_string(x = x_var, y = y_var, color = color_var)) + 
    geom_point() + ggtitle(plot_title) + theme_bw()
}

# Function for k-means clustering
perform_kmeans_clustering <- function(data, num_clusters) {
  set.seed(123)  # Set seed for reproducibility
  data_scaled <- scale(data %>% select(Age, Income, Spending_Score))  # Scale the data for clustering
  kmeans_result <- kmeans(data_scaled, centers = num_clusters)  # Apply k-means clustering
  data$Cluster <- as.factor(kmeans_result$cluster)
  print("K-means Clustering Results:")
  print(kmeans_result)
  ggplot(data, aes(x = Income, y = Spending_Score, color = Cluster)) + 
    geom_point() + ggtitle('Income vs Spending Score by Cluster') + theme_bw()
}

# Main function to execute the analysis
main <- function() {
  file_path <- '/Users/andrew/Downloads/Shopping_data.csv'
  data <- load_and_inspect_data(file_path)
  data <- clean_data(data)
  data <- transform_data(data)
  
  correlation_analysis(data)
  create_histograms(data)
  create_boxplots(data, 'Gender')
  create_boxplots(data, 'Rating')
  
  calculate_and_plot_averages(data, 'Gender', 'Income', 'darkorange', 'Average Income by Gender')
  calculate_and_plot_averages(data, 'Gender', 'Spending_Score', 'darkred', 'Average Spending Score by Gender')
  calculate_and_plot_averages(data, 'Rating', 'Income', 'darkgreen', 'Average Income by Rating')
  calculate_and_plot_averages(data, 'Rating', 'Spending_Score', 'purple', 'Average Spending Score by Rating')
  
  create_scatter_plots(data, 'Age', 'Income', 'Gender', 'Age vs Income by Gender')
  create_scatter_plots(data, 'Income', 'Spending_Score', 'Gender', 'Income vs Spending Score by Gender')
  create_scatter_plots(data, 'Age', 'Income', 'Rating', 'Age vs Income by Rating')
  create_scatter_plots(data, 'Income', 'Spending_Score', 'Rating', 'Income vs Spending Score by Rating')
  
  perform_kmeans_clustering(data, 3)
}

# Run the main function
main()

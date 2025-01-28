# Loading required libraries for Operations
if (!require('readxl')) {install.packages('readxl'); library(readxl)}
if (!require('dplyr')) {install.packages('dplyr'); library(dplyr)}

# Setting the path of Excel file
file_path <- "F:/Semester 4/Prob and Statistics/Project/i222655_i2222621_Inflation in Germany Data.xlsx"




# Task 2
# Loading data from the Excel file
data <- read_excel(file_path)
# Displaying Data is Separate File
View(data)
# Loading required libraries
if (!require('dplyr')) {install.packages('dplyr'); library(dplyr)}
if (!require('modeest')) {install.packages('modeest'); library(modeest)}
# Function to Calculate Mode
calculate_mode <- function(x) {
  ux <- unique(x)
  freq <- tabulate(match(x, ux))
  ux[freq == max(freq)][1]  # Return the first mode in case of ties
}
# Calculating Summary Statistics
summary_stats <- data %>%
  summarise(
    Inflation_Rate_Mean = mean(Inflation_Rate_in_Germany, na.rm = TRUE),
    Inflation_Rate_Median = median(Inflation_Rate_in_Germany, na.rm = TRUE),
    Inflation_Rate_Mode = calculate_mode(Inflation_Rate_in_Germany),
    Inflation_Rate_Q1 = quantile(Inflation_Rate_in_Germany, 0.25, na.rm = TRUE),
    Inflation_Rate_Q3 = quantile(Inflation_Rate_in_Germany, 0.75, na.rm = TRUE),
    
    Implied_Conversion_Rate_Mean = mean(Implied_Conversion_Rate, na.rm = TRUE),
    Implied_Conversion_Rate_Median = median(Implied_Conversion_Rate, na.rm = TRUE),
    Implied_Conversion_Rate_Mode = calculate_mode(Implied_Conversion_Rate),
    Implied_Conversion_Rate_Q1 = quantile(Implied_Conversion_Rate, 0.25, na.rm = TRUE),
    Implied_Conversion_Rate_Q3 = quantile(Implied_Conversion_Rate, 0.75, na.rm = TRUE),
    
    Government_Expenditure_Mean = mean(Government_Expenditure, na.rm = TRUE),
    Government_Expenditure_Median = median(Government_Expenditure, na.rm = TRUE),
    Government_Expenditure_Mode = calculate_mode(Government_Expenditure),
    Government_Expenditure_Q1 = quantile(Government_Expenditure, 0.25, na.rm = TRUE),
    Government_Expenditure_Q3 = quantile(Government_Expenditure, 0.75, na.rm = TRUE),
    
    Government_Gross_Dept_Mean = mean(Government_Gross_Dept, na.rm = TRUE),
    Government_Gross_Dept_Median = median(Government_Gross_Dept, na.rm = TRUE),
    Government_Gross_Dept_Mode = calculate_mode(Government_Gross_Dept),
    Government_Gross_Dept_Q1 = quantile(Government_Gross_Dept, 0.25, na.rm = TRUE),
    Government_Gross_Dept_Q3 = quantile(Government_Gross_Dept, 0.75, na.rm = TRUE)
  )
# Printing the summary statistics
print(summary_stats, width = Inf)




# Task 3
# Loading required libraries
if (!require('readxl')) {install.packages('readxl'); library(readxl)}
if (!require('dplyr')) {install.packages('dplyr'); library(dplyr)}
if (!require('ggplot2')) {install.packages('ggplot2'); library(ggplot2)}
if (!require('tidyr')) {install.packages('tidyr'); library(tidyr)}  
# Reshaping the data for plotting
data_long <- data %>%
  pivot_longer(
    cols = c("Inflation_Rate_in_Germany", "Implied_Conversion_Rate", "Government_Expenditure", "Government_Gross_Dept"),
    names_to = "Variable",
    values_to = "Value"
  )
# Constructing Box and Whisker Plots with Axis Lines
ggplot(data_long, aes(x = Variable, y = Value, fill = Variable)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 8) +
  theme_minimal() +
  labs(title = "Box and Whisker Plots of Economic Indicators", x = "Economic Indicator", y = "Value") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.line = element_line(colour = "black"))

# Task 3: Reshaping data for plotting
if (!require('tidyr')) {
  install.packages('tidyr')
  library(tidyr)
}




# Task 4
# Loading the required libraries
if (!require('ggplot2')) {install.packages('ggplot2'); library(ggplot2)}
if (!require('gridExtra')) {install.packages('gridExtra'); library(gridExtra)}

# Creating individual scatter plots
plot1 <- ggplot(data, aes(x = Inflation_Rate_in_Germany, y = Implied_Conversion_Rate)) + 
  geom_point(color = "blue") + 
  labs(title = "Inflation Rate vs Implied Conversion Rate", x = "Inflation Rate", y = "Implied Conversion Rate") +
  theme_minimal() +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1))

plot2 <- ggplot(data, aes(x = Inflation_Rate_in_Germany, y = Government_Expenditure)) + 
  geom_point(color = "blue") + 
  labs(title = "Inflation Rate vs Government Expenditure", x = "Inflation Rate", y = "Government Expenditure") +
  theme_minimal() +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1))

plot3 <- ggplot(data, aes(x = Inflation_Rate_in_Germany, y = Government_Gross_Dept)) + 
  geom_point(color = "blue") + 
  labs(title = "Inflation Rate vs Government Gross Dept", x = "Inflation Rate", y = "Government Gross Dept") +
  theme_minimal() +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1))

plot4 <- ggplot(data, aes(x = Government_Expenditure, y = Government_Gross_Dept)) + 
  geom_point(color = "blue") + 
  labs(title = "Government Expenditure vs Government Gross Dept", x = "Government Expenditure", y = "Government Gross Dept") +
  theme_minimal() +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1))

plot5 <- ggplot(data, aes(x = Implied_Conversion_Rate, y = Government_Expenditure)) + 
  geom_point(color = "blue") + 
  labs(title = "Implied Conversion Rate vs Government Expenditure", x = "Implied Conversion Rate", y = "Government Expenditure") +
  theme_minimal() +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1))

plot6 <- ggplot(data, aes(x = Implied_Conversion_Rate, y = Government_Gross_Dept)) + 
  geom_point(color = "blue") + 
  labs(title = "Implied Conversion Rate vs Government Gross Dept", x = "Implied Conversion Rate", y = "Government Gross Dept") +
  theme_minimal() +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1))

# Arranging all the plots on one grid
grid_plot <- gridExtra::grid.arrange(plot1, plot2, plot3, plot4, plot5, plot6, ncol = 3)
# Printing the grid of plots
print(grid_plot)




# Task 5
# Loading Library for regression analysis
if (!require('stats')) {install.packages('stats'); library(stats)}
# Running a linear regression model
model <- lm(Inflation_Rate_in_Germany ~ Implied_Conversion_Rate + Government_Expenditure + Government_Gross_Dept, data = data)
# Displaying the Summary of Regression Model
summary(model)


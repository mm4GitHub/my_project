
install.packages("tidyverse")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("reshape2")
install.packages("corrplot")

library(tidyverse)
library(dplyr)
library(corrplot)
library(ggplot2)
library(reshape2)

#Specifying and Reading the CSv file into a data frame
file_path <- "C:/Users/HP/Desktop/ASSIGNMENT/HealthInsuranceCoverage.csv"

HealthInsuranceData <- read.csv(file_path)

#Check for the structure of the data frame
str(HealthInsuranceData)

#load first 6 Data from the Data Frame
head(HealthInsuranceData)

#Check for the number of rows
num_rows <- nrow(HealthInsuranceData)
print(num_rows)

# Filter columns for the Group variable
filtered_Data <- HealthInsuranceData %>% 
  filter(Group %in% c("By Age", "By Sex"))
# View the new dataset
filtered_Data

# Dropping unwanted variables
HealthInsuranceData1 <- filtered_Data %>% 
  select(c("Indicator", "Group", "State", "Subgroup", "Time.Period.Label", "Value"))
#view the new dataset
print(HealthInsuranceData1)

# Checking for missing values
sum(is.na(HealthInsuranceData1))

#check summary statistics
summary(HealthInsuranceData1)

# Mean imputation for filling missing values
HealthInsuranceData_Cleaned$Value[is.na(HealthInsuranceData1$Value)] <- mean(HealthInsuranceData1$Value, na.rm = TRUE)

#check summary statistics of cleaned data
summary(HealthInsuranceData_Cleaned)

#check to validate the missing value is filled
sum(is.na(HealthInsuranceData_Cleaned))

#compare summary statistics
summary(HealthInsuranceData1)
summary(HealthInsuranceData_Cleaned)

print(HealthInsuranceData_Cleaned)


# Create a boxplot to visualize outliers
ggplot(HealthInsuranceData_Cleaned, aes(x = Indicator, y = Value)) +
  geom_boxplot(outlier.color = "red", outlier.size = 2) +
  labs(title = "Boxplot of Value by Indicator",
       x = "Indicator", y = "Value") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Summary Statistics for Key Variables
summary_stats <- HealthInsuranceData_Cleaned %>%
  group_by(Indicator, Group, Subgroup) %>%
  summarise(
    mean_value = mean(Value, na.rm = TRUE),
    median_value = median(Value, na.rm = TRUE),
    sd_value = sd(Value, na.rm = TRUE),
    min_value = min(Value, na.rm = TRUE),
    max_value = max(Value, na.rm = TRUE),
    n = n()  # Count of observations
  )
print(summary_stats)

# Visualize Distributions Using Histograms
ggplot(HealthInsuranceData_Cleaned, aes(x = Value)) +
  geom_histogram(binwidth = 5, fill = "skyblue", color = "black") +
  facet_wrap(~ Indicator, scales = "free") +
  labs(title = "Distribution of Value by Indicator",
       x = "Value", y = "Frequency") +
  theme_minimal()

# Boxplots to Compare Distributions Across Groups
ggplot(HealthInsuranceData_Cleaned, aes(x = Subgroup, y = Value, fill = Group)) +
  geom_boxplot() +
  facet_wrap(~ Indicator, scales = "free") +
  labs(title = "Boxplot of Value by Subgroup and Group",
       x = "Subgroup", y = "Value") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Line Plot to Identify Trends Over Time
ggplot(HealthInsuranceData_Cleaned, aes(x = Time.Period.Label, y = Value, color = Indicator)) +
  geom_line() +
  geom_point() +
  labs(title = "Trends in Value Over Time by Indicator",
       x = "Time Period", y = "Value") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Bar Plot to Compare Mean Values Across Categories
mean_plot_data <- HealthInsuranceData_Cleaned %>%
  group_by(Indicator, Subgroup) %>%
  summarise(mean_value = mean(Value, na.rm = TRUE))
ggplot(mean_plot_data, aes(x = Subgroup, y = mean_value, fill = Indicator)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Mean Value by Subgroup and Indicator",
       x = "Subgroup", y = "Mean Value") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#code for ANOVA to analyze if there is Significant differences in "Value" Variables
#across different age categories(Subgroup) within specific group(By Age)
# Filter rows for the group of interest (e.g., "By Age")
filtered_age <- HealthInsuranceData_Cleaned %>%
  filter(Group == "By Age")

# Perform normality tests for each Subgroup
normality_test <- filtered_age %>%
  group_by(Subgroup) %>%  # Group by Subgroup
  summarise(
    p_value = shapiro.test(Value)$p.value,  # Perform Shapiro-Wilk test for each subgroup
    #mean_value = mean(Value, na.rm = TRUE)  # Optional: Calculate mean for reference
  )

# Print the results of the normality test
print(normality_test)

# Homogeneity of Variance: Levene's test
install.packages("car")
library(car)
levenes_test <- leveneTest(Value ~ Subgroup, data = filtered_age)
print("Levene's Test Results:")
print(levenes_test)

# Perform one-way ANOVA
anova_age <- aov(Value ~ Subgroup, data = filtered_age)
summary(anova_age)

# Visualize the results
library(ggplot2)
ggplot(filtered_age, aes(x = Subgroup, y = Value)) +
  geom_boxplot() +
  labs(title = "Boxplot of Value by Subgroup",
       x = "Subgroup", y = "Value") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



#code for ANOVA to analyze if there is Significant differences in the mean of the "Value" Variables
#across the three categories in the Indicator Column
#Filter rows for the three Indicator categories of interest
filtered_indicator <- HealthInsuranceData_Cleaned %>%
  filter (Indicator %in% c("Uninsured at the Time of Interview",
                           "Public Health Insurance Coverage",
                           "Private Health Insurance Coverage"))

# Perform normality tests for each Subgroup
normality_test <- filtered_indicator %>%
  group_by(Indicator %in% c("Uninsured at the Time of Interview",
                            "Public Health Insurance Coverage",
                            "Private Health Insurance Coverage")) %>%  # Group by Subgroup
  summarise(
    p_value = shapiro.test(Value)$p.value,  # Perform Shapiro-Wilk test for each subgroup
  )

# Print the results of the normality test
print(normality_test)

# Homogeneity of Variance: Levene's test
levenes_test <- leveneTest(Value ~ Subgroup, data = filtered_indicator)
print("Levene's Test Results:")
print(levenes_test)

# Perform one-way ANOVA
anova_indicator <- aov(Value ~ Subgroup, data = filtered_indicator)
summary(anova_indicator)

# Visualize the results
library(ggplot2)
ggplot(filtered_indicator, aes(x = Indicator, y = Value)) +
  geom_boxplot() +
  labs(title = "Boxplot of Value by Indicator",
       x = "Indicator", y = "Value") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#code to calculate the Confidence Interval for the three categories of the Indicator
#column using Z-test with 95% confidence level.
# Filter rows for the three Indicator categories of interest
filtered_Indicator <- HealthInsuranceData_Cleaned %>%
  filter(Indicator %in% c("Uninsured at the Time of Interview", 
                          "Public Health Insurance Coverage", 
                          "Private Health Insurance Coverage"))

# Function to calculate confidence interval using Z-test
calculate_CI <- function(values, confidence_level = 0.95) {
  n <- length(values)  # Dynamically calculate sample size for the group
  mean_val <- mean(values, na.rm = TRUE)  # Mean
  sd_val <- sd(values, na.rm = TRUE)  # Standard deviation
  
  if (is.na(sd_val) || sd_val == 0) {
    # Handle cases where standard deviation is NA or zero
    return(c(mean = mean_val, lower = NA, upper = NA))
  }
  
  se <- sd_val / sqrt(n)  # Standard error
  
  # Z-score for the given confidence level (default: 95%)
  z_score <- qnorm((1 + confidence_level) / 2)
  
  # Calculate confidence interval
  lower_bound <- mean_val - z_score * se
  upper_bound <- mean_val + z_score * se
  
  return(c(mean = mean_val, lower = lower_bound, upper = upper_bound))
}

# Calculate confidence intervals for each Indicator category
confidence_intervals <- filtered_Indicator %>%
  group_by(Indicator) %>%
  summarise(
    mean = mean(Value, na.rm = TRUE),          # Mean of the group
    sd = sd(Value, na.rm = TRUE),             # Standard deviation of the group
    n = n(),                             # Sample size for the group
    ci = list(calculate_CI(Value)),           # Calculate CI for each group
    lower = ci[[1]]["lower"],                 # Extract lower bound
    upper = ci[[1]]["upper"]                  # Extract upper bound
  ) %>%
  select(-ci)  # Remove the intermediate list column

# Print the results
print(confidence_intervals)

library(ggplot2)
ggplot(confidence_intervals, aes(x = Indicator, y = mean)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, color = "red") +
  labs(title = "Confidence Intervals for Indicator Categories",
       x = "Indicator", y = "Mean Value")








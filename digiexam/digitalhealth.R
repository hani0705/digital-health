# Install necessary packages if not already installed
if (!require("dplyr")) install.packages("dplyr")
if (!require("readr")) install.packages("readr")
if (!require("ggplot2")) install.packages("ggplot2")

# Load the required libraries
library(dplyr)
library(readr)
library(ggplot2)

# Load the data
data <- read_csv("C:/Users/Hani.abdi/Desktop/digiexam/diabetics.csv")

# Display the first few rows of the data
head(data)

# Inspect the structure of the data
str(data)

# Display summary statistics of the data
summary(data)

# Convert columns to appropriate data types
cleaned_data <- data %>%
  mutate(
    gender = as.factor(gender),
    age = as.numeric(gsub("[^0-9.]", "", age)),  # Clean age column by extracting numeric values
    hypertension = as.factor(hypertension),
    heart_disease = as.factor(heart_disease),
    smoking_history = as.factor(smoking_history),
    bmi = as.numeric(bmi),
    HbA1c_level = as.numeric(HbA1c_level),
    blood_glucose_level = as.numeric(blood_glucose_level),
    diabetes = as.factor(diabetes)
  )

# Handle missing values
cleaned_data <- cleaned_data %>%
  filter(complete.cases(.))

# Convert gender to lowercase and standardize levels (assuming gender is already a factor)
cleaned_data$gender <- tolower(as.character(cleaned_data$gender))
cleaned_data$gender <- factor(cleaned_data$gender)

# Recode hypertension levels to numeric (NO to 0, yes to 1)
cleaned_data$hypertension <- ifelse(cleaned_data$hypertension == "NO", 0, 1)

# Convert diabetes column to numeric (no to 0, yes to 1)
cleaned_data$diabetes <- ifelse(cleaned_data$diabetes == "no", 0, 1)

# Display summary statistics of the cleaned data
summary(cleaned_data)

# Visualize data to further validate cleanliness
# Example: Histogram of age
ggplot(cleaned_data, aes(x = age)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black") +
  labs(title = "Distribution of Age", x = "Age", y = "Count")

# Save cleaned data to the same path
write_csv(cleaned_data, "C:/Users/Hani.abdi/Desktop/digiexam/cleaned_diabetics.csv")

# Additional visualizations and checks as needed



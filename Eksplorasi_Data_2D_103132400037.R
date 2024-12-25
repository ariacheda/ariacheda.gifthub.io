# Load required libraries
library(tidyverse)
library(ggplot2)
library(gridExtra)

# Read the data
diabetes_data <- read.csv("diabetes_prediction_dataset.csv")

view(diabetes_data)

# Take 30 rows of data at random
set.seed(123) # Untuk reproducibility (opsional, tapi disarankan)
random_indices <- sample(1:nrow(diabetes_data), 30, replace = FALSE)
data_30_random <- diabetes_data[random_indices, ]

view(data_30_random)

# Create scatter plots with correlations
# 1. HbA1c vs Diabetes
plot1 <- ggplot(data_30_random, aes(x=HbA1c_level, y=diabetes)) +
  geom_point(color="#FF6B6B", size=3) +
  geom_smooth(method=lm, color="#4ECDC4") +
  labs(title="HbA1c vs Diabetes",
       x="HbA1c", y="Diabetes") +
  theme_minimal()

# 2. Age vs BMI
plot2 <- ggplot(data_30_random, aes(x=age, y=bmi)) +
  geom_point(color="#45B7D1", size=3) +
  geom_smooth(method=lm, color="#96CEB4") +
  labs(title="Age vs BMI",
       x="Age", y="BMI") +
  theme_minimal()

# 3. BMI vs Glucose
plot3 <- ggplot(data_30_random, aes(x=bmi, y=blood_glucose_level)) +
  geom_point(color="#D65076", size=3) +
  geom_smooth(method=lm, color="#EEB868") +
  labs(title="BMI vs Blood Glucose Level",
       x="BMI", y="Blood Glucose Level") +
  theme_minimal()

# Calculate correlations
cor_HbA1c_diabetes <- cor(data_30_random$HbA1c_level, data_30_random$diabetes)
cor_age_bmi <- cor(data_30_random$age, data_30_random$bmi)
cor_bmi_glucose <- cor(data_30_random$bmi, data_30_random$blood_glucose_level)

# Display plots in a grid
grid.arrange(plot1, plot2, plot3, ncol=2)

# Print correlations
cat("Correlations:\n",
    "HbA1c-Diabetes:", round(cor_HbA1c_diabetes, 3), "\n",
    "Age-BMI:", round(cor_age_bmi, 3), "\n",
    "BMI-Glucose:", round(cor_bmi_glucose, 3))

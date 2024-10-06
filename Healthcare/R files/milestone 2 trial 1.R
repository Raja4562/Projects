# Read the dataset
healthcare_data <- read.csv("healthcare_dataset.csv", stringsAsFactors = FALSE)

# Load required libraries
library(ggplot2)
library(lubridate)

# Convert date columns to Date format
healthcare_data$Date.of.Admission <- ymd(healthcare_data$Date.of.Admission)
healthcare_data$Discharge.Date <- ymd(healthcare_data$Discharge.Date)

# Calculate length of stay
healthcare_data$Length.of.Stay <- healthcare_data$Discharge.Date - healthcare_data$Date.of.Admission

# Create tile plot
ggplot(healthcare_data, aes(x = Length.of.Stay, y = Medical.Condition)) +
  geom_bin2d() +
  labs(x = "Length of Hospital Stay (Days)", y = "Medical Condition", title = "Length of Hospital Stay by Medical Condition") +
  theme_minimal()


# Create line graph with smoothing
ggplot(healthcare_data, aes(x = Date.of.Admission, y = Length.of.Stay, color = Insurance.Provider)) +
  geom_smooth() +
  labs(x = "Date of Admission", y = "Length of Hospital Stay (Days)", title = "Length of Hospital Stay over Time by Insurance Provider") +
  theme_minimal()

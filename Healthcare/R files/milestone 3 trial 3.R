# Load your healthcare dataset
healthcare_data <- read.csv("healthcare_dataset.csv", stringsAsFactors = FALSE)

# Assuming Date.of.Admission is in Date format
healthcare_data$Date.of.Admission <- as.Date(healthcare_data$Date.of.Admission, format = "%Y-%m-%d")

# Calculate length of stay
healthcare_data$Length.of.Stay <- as.numeric(difftime(healthcare_data$Discharge.Date, healthcare_data$Date.of.Admission, units = "days"))

# Assuming Length.of.Stay is the variable you want to visualize
var_to_visualize <- healthcare_data$Length.of.Stay

# Execute the script with your data
source("calendarHeat.R")
calendarHeat(healthcare_data$Date.of.Admission, var_to_visualize, varname = "Length of Stay")

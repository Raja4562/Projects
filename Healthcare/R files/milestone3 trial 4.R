# Load your healthcare dataset
healthcare_data <- read.csv("healthcare_dataset.csv", stringsAsFactors = FALSE)

# Assuming Date.of.Admission is in Date format
healthcare_data$Date.of.Admission <- as.Date(healthcare_data$Date.of.Admission, format = "%Y-%m-%d")

# Aggregate data to get the count of people admitted on each day
admission_counts <- table(healthcare_data$Date.of.Admission)

# Convert result to a data frame
admission_counts_df <- data.frame(Date = as.Date(names(admission_counts)), Count = as.numeric(admission_counts))

# Execute the script with your data
source("calendarHeat.R")
calendarHeat(admission_counts_df$Date, admission_counts_df$Count, varname = "Number of Admissions")



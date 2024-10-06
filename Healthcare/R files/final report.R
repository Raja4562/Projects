library(dplyr)
library(lubridate)
library(ggplot2)

# Read dataset
healthcare_data <- read.csv("healthcare_dataset_final.csv", stringsAsFactors = FALSE)

# Convert Date of Admission to Date format
healthcare_data$Date.of.Admission <- as.Date(healthcare_data$Date.of.Admission, format = "%Y-%m-%d")

# Extract Admission Year
healthcare_data <- healthcare_data %>%
  mutate(AdmissionYear = year(Date.of.Admission))

# Create Mosaic Table
mosaic_table <- table(healthcare_data$AdmissionYear, healthcare_data$Medical.Condition)

# Define Color Palette for Mosaic Plot
color_palette <- c(
  "Obesity" = "#00B050",
  "Hypertension" = "#92D050",
  "Diabetes" = "#FFFF00",
  "Cancer" = "#FF9900",
  "Asthma" = "#FF6600",
  "Arthritis" = "#FF0000"
)

# Plot Mosaic Plot with Axis Labels
mosaicplot(mosaic_table, 
           main = "Medical Condition by Year of Admission",
           color = color_palette, 
           legend = TRUE,
           xlab = "Year of Admission",   # Add X-axis label
           ylab = "Medical Condition")   # Add Y-axis label

# Generate Calendar Heatmap
admission_counts <- table(healthcare_data$Date.of.Admission)
admission_counts_df <- data.frame(Date = as.Date(names(admission_counts)), Count = as.numeric(admission_counts))

source("calendarHeat.R")
calendarHeat(admission_counts_df$Date, admission_counts_df$Count, varname = "Number of Admissions")

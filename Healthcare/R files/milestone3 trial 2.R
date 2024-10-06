# Assuming healthcare_data is your processed dataset with Date.of.Admission, Discharge.Date, and Length.of.Stay columns
healthcare_data <- read.csv("healthcare_dataset.csv", stringsAsFactors = FALSE)
# Load required libraries
library(ggplot2)
library(lubridate)

# Calculate length of stay
healthcare_data$Length.of.Stay <- as.numeric(difftime(healthcare_data$Discharge.Date, healthcare_data$Date.of.Admission, units = "days"))

# Extract Year and Month for grouping
healthcare_data$YearMonth <- format(healthcare_data$Date.of.Admission, "%Y-%m")

# Create calendar plot
ggplot(healthcare_data, aes(x = as.Date(Date.of.Admission), y = factor(day(Date.of.Admission)), fill = Length.of.Stay)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "lightblue", high = "darkblue", guide = "colorbar") +
  labs(title = "Length of Hospital Stay Calendar Plot",
       x = "Date of Admission",
       y = "Day of Month",
       fill = "Length of Stay (Days)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Check the structure of the data
str(healthcare_data)

# Print the first few rows of the data
head(healthcare_data)


# Convert to date format
healthcare_data$Date.of.Admission <- as.Date(healthcare_data$Date.of.Admission, format = "%d-%m-%Y")
healthcare_data$Discharge.Date <- as.Date(healthcare_data$Discharge.Date, format = "%d-%m-%Y")


# Check for missing values
summary(is.na(healthcare_data$Date.of.Admission))
summary(is.na(healthcare_data$Length.of.Stay))


subset_data <- subset(healthcare_data, Medical.Condition == "YourCondition")
ggplot(subset_data, aes(x = as.Date(Date.of.Admission), y = factor(day(Date.of.Admission)), fill = Length.of.Stay)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "lightblue", high = "darkblue", guide = "colorbar") +
  labs(title = "Length of Hospital Stay Calendar Plot",
       x = "Date of Admission",
       y = "Day of Month",
       fill = "Length of Stay (Days)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Convert to date format with the correct format
healthcare_data$Date.of.Admission <- as.Date(healthcare_data$Date.of.Admission, format = "%Y-%m-%d")
healthcare_data$Discharge.Date <- as.Date(healthcare_data$Discharge.Date, format = "%Y-%m-%d")

# Check for missing values again
summary(is.na(healthcare_data$Date.of.Admission))
summary(is.na(healthcare_data$Length.of.Stay))


ggplot(healthcare_data, aes(x = as.Date(Date.of.Admission), y = factor(day(Date.of.Admission)), fill = Length.of.Stay)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "lightblue", high = "darkblue", guide = "colorbar") +
  labs(title = "Length of Hospital Stay Calendar Plot",
       x = "Date of Admission",
       y = "Day of Month",
       fill = "Length of Stay (Days)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Check the structure of the data
str(healthcare_data)

# Check for missing values and non-finite values in Date.of.Admission
summary(is.na(healthcare_data$Date.of.Admission))
summary(!is.finite(healthcare_data$Date.of.Admission))


# Print a few rows of the data to inspect the Date.of.Admission column
head(healthcare_data$Date.of.Admission)

# Check for unique values in the Date.of.Admission column
unique(healthcare_data$Date.of.Admission)

# Check for any patterns or issues in the dataset related to Date.of.Admission


#Section 1: Loading in the Data File and R libraries. 

#Reading in the libraries

library("readxl")
library("dplyr")
library("tidyverse")
library("writexl")
library(ggplot2)
library(lubridate)
library(tidyquant)
library(stringr)
library(tidyr)
library(anytime)
library(forecast)


data <- read_excel("C:/Users/amrigupt/Downloads/whole2024ChangeEvents.xlsx")
head(data)


# Check for NA values in EVENT_OCCUR_DATE after conversion
print(sum(is.na(data$EVENT_OCCUR_DATE)))

# Print out the first few entries of the date column to confirm proper date parsing
print(head(data$EVENT_OCCUR_DATE))

# Print unique EVENT_NAME values to verify correct filtering
print(unique(data$EVENT_NAME))

#New Relationship Events Analysis

# Filter data to include only "NEW_RELATIONSHIP" events
filtered_data_new_relationship<- data %>%filter(EVENT_NAME == "New_Relationship")

# Print the filtered data to check if it's empty
print(head(filtered_data_new_relationship))

# Extract year and month from the 'EVENT_OCCUR_DATE' column
filtered_data_new_relationship <- filtered_data_new_relationship %>%mutate(Year = year(EVENT_OCCUR_DATE),Month = month(EVENT_OCCUR_DATE, label = TRUE)) # Use label = TRUE for month names

# Check for NA values in Year and Month columns
print(sum(is.na(filtered_data_new_relationship$Year)))
print(sum(is.na(filtered_data_new_relationship$Month)))

# Print the first few rows to verify correct extraction
print(head(filtered_data_new_relationship))

# Group by 'CATEGORY_NAME', 'Year', and 'Month' and count the number of events
summary_data_new_relationship <- filtered_data_new_relationship %>%
    group_by(CATEGORY_NAME, Year, Month) %>%
    summarise(NumberOfEvents = n(), .groups = 'drop') %>%
    arrange(CATEGORY_NAME, Year, Month)  # Arrange data for clarity

# Print the summary data to check for correctness
print(summary_data_new_relationship, n=130)


#Inactivate_Relationship Change Analysis

filtered_data_inactivate_relationship<- data %>%filter(EVENT_NAME == "Inactivate_Relationship")

# Print the filtered data to check if it's empty
print(head(filtered_data_inactivate_relationship))

# Extract year and month from the 'EVENT_OCCUR_DATE' column
filtered_data_inactivate_relationship <- filtered_data_inactivate_relationship %>%mutate(Year = year(EVENT_OCCUR_DATE),Month = month(EVENT_OCCUR_DATE, label = TRUE)) # Use label = TRUE for month names

# Check for NA values in Year and Month columns
print(sum(is.na(filtered_data_inactivate_relationship$Year)))
print(sum(is.na(filtered_data_inactivate_relationship$Month)))

# Print the first few rows to verify correct extraction
print(head(filtered_data_inactivate_relationship))

# Group by 'CATEGORY_NAME', 'Year', and 'Month' and count the number of events
summary_data_inactivate_relationship <- filtered_data_inactivate_relationship %>%
    group_by(CATEGORY_NAME, Year, Month) %>%
    summarise(NumberOfEvents = n(), .groups = 'drop') %>%
    arrange(CATEGORY_NAME, Year, Month)  # Arrange data for clarity

# Print the summary data to check for correctness
print(summary_data_inactivate_relationship)



#Update_Relationships Change Analysis


filtered_data_Update_Relationship<- data %>%filter(EVENT_NAME == "Update_Relationship")

# Print the filtered data to check if it's empty
print(head(filtered_data_Update_Relationship))

# Extract year and month from the 'EVENT_OCCUR_DATE' column
filtered_data_Update_Relationship <- filtered_data_Update_Relationship %>%mutate(Year = year(EVENT_OCCUR_DATE),Month = month(EVENT_OCCUR_DATE, label = TRUE)) # Use label = TRUE for month names

# Check for NA values in Year and Month columns
print(sum(is.na(filtered_data_Update_Relationship$Year)))
print(sum(is.na(filtered_data_Update_Relationship$Month)))

# Print the first few rows to verify correct extraction
print(head(filtered_data_Update_Relationship))

# Group by 'CATEGORY_NAME', 'Year', and 'Month' and count the number of events
summary_data_Update_Relationship <- filtered_data_Update_Relationship %>%
    group_by(CATEGORY_NAME, Year, Month) %>%
    summarise(NumberOfEvents = n(), .groups = 'drop') %>%
    arrange(CATEGORY_NAME, Year, Month)  # Arrange data for clarity

# Print the summary data to check for correctness
print(summary_data_Update_Relationship)



#New Node Analysis

filtered_data_New_Node<- data %>%filter(EVENT_NAME == "New_Node")

# Print the filtered data to check if it's empty
print(head(filtered_data_New_Node))

# Extract year and month from the 'EVENT_OCCUR_DATE' column
filtered_data_New_Node <- filtered_data_New_Node %>%mutate(Year = year(EVENT_OCCUR_DATE),Month = month(EVENT_OCCUR_DATE, label = TRUE)) # Use label = TRUE for month names

# Check for NA values in Year and Month columns
print(sum(is.na(filtered_data_New_Node$Year)))
print(sum(is.na(filtered_data_New_Node$Month)))

# Print the first few rows to verify correct extraction
print(head(filtered_data_New_Node))

# Group by 'CATEGORY_NAME', 'Year', and 'Month' and count the number of events
summary_data_New_Node <- filtered_data_New_Node %>%
    group_by(CATEGORY_NAME, Year, Month) %>%
    summarise(NumberOfEvents = n(), .groups = 'drop') %>%
    arrange(CATEGORY_NAME, Year, Month)  # Arrange data for clarity

# Print the summary data to check for correctness
print(summary_data_New_Node)


#Inactivate Node Analysis

filtered_data_Inactivate_Node<- data %>%filter(EVENT_NAME == "Inactivate_Node")

# Print the filtered data to check if it's empty
print(head(filtered_data_Inactivate_Node))

# Extract year and month from the 'EVENT_OCCUR_DATE' column
filtered_data_Inactivate_Node <- filtered_data_Inactivate_Node %>%mutate(Year = year(EVENT_OCCUR_DATE),Month = month(EVENT_OCCUR_DATE, label = TRUE)) # Use label = TRUE for month names

# Check for NA values in Year and Month columns
print(sum(is.na(filtered_data_Inactivate_Node$Year)))
print(sum(is.na(filtered_data_Inactivate_Node$Month)))

# Print the first few rows to verify correct extraction
print(head(filtered_data_Inactivate_Node))

# Group by 'CATEGORY_NAME', 'Year', and 'Month' and count the number of events
summary_data_Inactivate_Node <- filtered_data_Inactivate_Node %>%
    group_by(CATEGORY_NAME, Year, Month) %>%
    summarise(NumberOfEvents = n(), .groups = 'drop') %>%
    arrange(CATEGORY_NAME, Year, Month)  # Arrange data for clarity

# Print the summary data to check for correctness
print(summary_data_Inactivate_Node)


#Update Node Analysis

filtered_data_Update_Node<- data %>%filter(EVENT_NAME == "Update_Node")
filtered_data_Update_Node

# Print the filtered data to check if it's empty
print(head(filtered_data_Update_Node))

# Extract year and month from the 'EVENT_OCCUR_DATE' column
filtered_data_Update_Node <- filtered_data_Update_Node %>%mutate(Year = year(EVENT_OCCUR_DATE),Month = month(EVENT_OCCUR_DATE, label = TRUE)) # Use label = TRUE for month names

# Check for NA values in Year and Month columns
print(sum(is.na(filtered_data_Update_Node$Year)))
print(sum(is.na(filtered_data_Update_Node$Month)))

# Print the first few rows to verify correct extraction
print(head(filtered_data_Update_Node))

# Group by 'CATEGORY_NAME', 'Year', and 'Month' and count the number of events
summary_data_Update_Node <- filtered_data_Update_Node %>%
    group_by(CATEGORY_NAME, Year, Month) %>%
    summarise(NumberOfEvents = n(), .groups = 'drop') %>%
    arrange(CATEGORY_NAME, Year, Month)  # Arrange data for clarity

# Print the summary data to check for correctness
print(summary_data_Update_Node)


#FORECASTING SECTION

#Forecasting for NUMBER OF NEW RELATIONSHIP CHANGE EVENTS

summary_data_new_relationship

# Install required packages
install.packages("forecast")
install.packages("dplyr")

# Load libraries
library(forecast)
library(dplyr)


# Clean the CATEGORY_NAME column by removing any leading/trailing whitespace
summary_data_new_relationship <- summary_data_new_relationship %>%
    mutate(CATEGORY_NAME = trimws(CATEGORY_NAME))

# Convert 'Year' and 'Month' into a proper date format
summary_data_new_relationship <- summary_data_new_relationship %>%
    mutate(
        Year = as.character(Year),    # Convert 'Year' to character
        Month = as.character(Month)   # Convert 'Month' to character
    ) %>%
    mutate(Date = as.Date(paste(Year, Month, "01", sep = "-"), format = "%Y-%B-%d"))

# Get unique categories
categories <- unique(summary_data_new_relationship$CATEGORY_NAME)

# Initialize a list to store forecast results
forecasts_list <- list()

# Loop through each category
for (category in categories) {
    
    # Filter data for the current category
    category_data <- summary_data_new_relationship %>%
        filter(CATEGORY_NAME == category)
    
    # Check if the filtered data has any rows
    if (nrow(category_data) == 0) {
        warning(paste("No data available for category:", category))
        next
    }
    
    # Check for missing values in NumberOfEvents
    if (any(is.na(category_data$NumberOfEvents))) {
        warning(paste("Missing values in NumberOfEvents for category:", category))
        next
    }
    
    # Sort the data by the date column to ensure proper time series order
    category_data <- category_data %>%
        arrange(Date)
    
    # Create a time series object from the NumberOfEvents column
    ts_data <- ts(category_data$NumberOfEvents, frequency = 12, 
                  start = c(min(as.numeric(category_data$Year)), min(as.numeric(format(category_data$Date, "%m")))))
    
    # Plot the original time series
    plot(ts_data, main = paste("Original Time Series for", category), ylab = "Number of Events", xlab = "Time")
    
    # Fit an ETS model to the time series
    fit <- ets(ts_data)
    
    # Forecast for the next 12 months
    forecast_data <- forecast(fit, h = 12)
    
    # Ensure no negative values by replacing negatives with zero in the forecast
    forecast_data$mean[forecast_data$mean < 0] <- 0
    forecast_data$lower[forecast_data$lower < 0] <- 0
    forecast_data$upper[forecast_data$upper < 0] <- 0
    
    # Store the forecast in the list
    forecasts_list[[category]] <- forecast_data
    
    # Plot the forecast with no negative values
    plot(forecast_data, main = paste("12-Month ETS Forecast for", category), ylab = "Number of Events", xlab = "Time")
}

# Optionally: print the forecast for each category
for (category in names(forecasts_list)) {
    cat("\nForecast for category:", category, "\n")
    print(forecasts_list[[category]])
}



# ==============================================================================
# SETUP AND PACKAGE LOADING
# ==============================================================================
# Set working directory to project root
setwd("/Users/satkarkarki/Desktop/Data_Analytics_Portfolio/eggs_forecasting")

# Install required packages if they are not already installed
# install.packages(c("tidyverse", "forecast", "ggfortify")) # Uncomment if needed

# Load necessary libraries
library(tidyverse)    # For data manipulation (dplyr, readr) and visualization (ggplot2)
library(forecast)     # For time series forecasting functions like ts() and autoplot()
library(ggfortify)    # For automatic plotting of time series objects with ggplot2

# ==============================================================================
# DATA IMPORT AND INITIAL CLEANING
# ==============================================================================
# Check if the inflation-adjusted data file exists
adjusted_data_path <- "/Users/satkarkarki/Desktop/Data_Analytics_Portfolio/eggs_forecasting/data/eggs_adjusted.csv"
original_data_path <- "/Users/satkarkarki/Desktop/Data_Analytics_Portfolio/eggs_forecasting/data/eggs.csv"

# Use the inflation-adjusted data if available, otherwise use the original data
if (file.exists(adjusted_data_path)) {
  data_path <- adjusted_data_path
  cat("Using inflation-adjusted egg price data\n")
  
  # Load the adjusted data and select only the date and real_price columns
  eggs <- readr::read_csv(data_path) %>%
    select(date, real_price) %>%
    rename(egg_price = real_price)
  
  cat("Using inflation-adjusted prices for analysis\n")
} else {
  cat("Using original egg price data (not inflation-adjusted)\n")
  
  # Load original data
  eggs <- readr::read_csv(original_data_path) %>%
    rename(
      date = observation_date,
      egg_price = APU0000708111
    )
}

# Check if the date column is properly formatted
if (is.logical(eggs$date)) {
  cat("WARNING: Date column may not be properly formatted. Fixing this issue...\n")
  
  # Reload the data and fix the date formatting
  original_eggs <- readr::read_csv(original_data_path)
  
  # Inspect the format of dates
  cat("Original date format examples:", head(original_eggs$observation_date, 3), "\n")
  
  # Try to convert using the correct format (likely m/d/y format)
  eggs <- original_eggs %>%
    mutate(
      date = as.Date(observation_date, format="%m/%d/%y")
    ) %>%
    rename(
      egg_price = APU0000708111
    ) %>%
    select(date, egg_price)
  
  # If using adjusted data, adjust the egg prices
  if (file.exists(adjusted_data_path)) {
    # Get the CPI data and calculate real prices
    cpi_data <- original_eggs %>% 
      select(observation_date, cpi) %>%
      rename(date = observation_date) %>%
      mutate(date = as.Date(date, format="%m/%d/%y"))
    
    # Join with the eggs data
    eggs <- eggs %>%
      inner_join(cpi_data, by = "date")
    
    # Calculate the inflation adjustment
    latest_cpi <- max(eggs$cpi, na.rm = TRUE)
    
    eggs <- eggs %>%
      mutate(egg_price = egg_price * (latest_cpi / cpi)) %>%
      select(date, egg_price)
      
    cat("Recalculated inflation-adjusted prices\n")
  }
}

# Verify column renaming and data structure
print("First few rows after processing:")
print(head(eggs))
print("Data structure after processing:")
print(str(eggs))

# Update the time series object to start from 2020
if (nrow(eggs) > 0) {
  start_year <- as.numeric(format(min(eggs$date), "%Y"))
  start_month <- as.numeric(format(min(eggs$date), "%m"))
  eggs_ts <- ts(eggs$egg_price,
                start = c(start_year, start_month),
                frequency = 12)
}

# ==============================================================================
# TIME SERIES OBJECT CREATION
# ==============================================================================
# Verify time series properties
print("First few values of the time series object:")
print(head(eggs_ts))
print(paste("Frequency of time series:", frequency(eggs_ts)))
print(paste("Class of time series object:", class(eggs_ts)))

# Optional: Check for missing values
print(paste("Any NA values in egg_price column?", anyNA(eggs$egg_price)))
print(paste("Any NA values in the time series object?", anyNA(eggs_ts)))

# The script now provides 'eggs' (dataframe) and 'eggs_ts' (time series object)
# for further analysis, using inflation-adjusted prices if available.

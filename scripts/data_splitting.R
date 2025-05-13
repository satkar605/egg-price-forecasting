# ==============================================================================
# DATA SPLITTING FOR MODEL EVALUATION
# ==============================================================================
# This script splits the egg price data into training and validation sets
# Now using inflation-adjusted prices for more meaningful analysis

# Set working directory to project root
setwd("/Users/satkarkarki/Desktop/Data_Analytics_Portfolio/eggs_forecasting")

# Load necessary libraries
library(tidyverse)    # For data manipulation and visualization
library(forecast)     # For time series functions
library(patchwork)    # For combining plots

# Create directory for visualizations
dir.create("visuals/2_data_splitting", showWarnings = FALSE, recursive = TRUE)

# Source the data loading script
source("scripts/load_data.R")

# Verify if inflation-adjusted prices are being used
cat("Using", ifelse(file.exists("data/eggs_adjusted.csv"), "inflation-adjusted", "nominal"), "egg prices for analysis\n")

# ==============================================================================
# 1. SPLITTING THE DATA
# ==============================================================================
# Determine the end date of the data
end_date <- max(eggs$date)
cat("End date of the dataset:", format(end_date, "%B %Y"), "\n")

# Calculate the validation start date (1 year before the end)
validation_start_date <- end_date - lubridate::years(1) + lubridate::days(1)
cat("Validation set starts from:", format(validation_start_date, "%B %Y"), "\n")

# Split the data frame
train_df <- eggs %>% 
  filter(date < validation_start_date)

validation_df <- eggs %>% 
  filter(date >= validation_start_date)

# Check the number of observations in each set
cat("Training set size:", nrow(train_df), "observations\n")
cat("Validation set size:", nrow(validation_df), "observations\n")

# Split the time series object
# Determine the exact time indices for splitting
total_obs <- length(eggs_ts)
n_validation <- 12  # 1 year of monthly data

# Split the time series
train_ts <- window(eggs_ts, end = time(eggs_ts)[total_obs - n_validation])
validation_ts <- window(eggs_ts, start = time(eggs_ts)[total_obs - n_validation + 1])

# Verify the split
cat("\nTime series split:\n")
cat("Training set: from", tsp(train_ts)[1], "to", tsp(train_ts)[2], "\n")
cat("Validation set: from", tsp(validation_ts)[1], "to", tsp(validation_ts)[2], "\n")
cat("Training set length:", length(train_ts), "observations\n")
cat("Validation set length:", length(validation_ts), "observations\n")

# ==============================================================================
# 2. VISUALIZING THE SPLIT
# ==============================================================================
# Create a data frame indicating which points belong to training vs validation
viz_data <- eggs %>%
  mutate(
    dataset = if_else(date < validation_start_date, "Training", "Validation")
  )

# Create visualization of the train/validation split
split_plot <- ggplot(viz_data, aes(x = date, y = egg_price, color = dataset)) +
  geom_line(size = 1) +
  scale_color_manual(
    values = c("Training" = "#2C3E50", "Validation" = "#E74C3C"),
    labels = c("Training Set", "Validation Set (1 Year)")
  ) +
  labs(
    title = "Train-Validation Split for Model Evaluation",
    subtitle = paste0("Training: ", format(min(eggs$date), "%b %Y"), 
                     " to ", format(max(train_df$date), "%b %Y"), 
                     " | Validation: ", format(min(validation_df$date), "%b %Y"), 
                     " to ", format(max(validation_df$date), "%b %Y")),
    y = "Price ($)",
    x = "Year",
    color = "Dataset"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11),
    axis.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    legend.title = element_text(face = "bold")
  ) +
  # Add a vertical line at the split point
  geom_vline(xintercept = as.numeric(validation_start_date), 
             linetype = "dashed", color = "darkgray")

# Display the visualization
print(split_plot)

# Save the visualization
ggsave("visuals/2_data_splitting/train_validation_split.png", split_plot, width = 10, height = 6, dpi = 300, bg = "white")

# Create a zoomed-in version focusing on recent years
zoom_years <- 5  # Show the last 5 years
zoom_start_date <- end_date - lubridate::years(zoom_years)

zoomed_plot <- ggplot(viz_data %>% filter(date >= zoom_start_date), 
                     aes(x = date, y = egg_price, color = dataset)) +
  geom_line(size = 1) +
  scale_color_manual(
    values = c("Training" = "#2C3E50", "Validation" = "#E74C3C"),
    labels = c("Training Set", "Validation Set (1 Year)")
  ) +
  labs(
    title = "Train-Validation Split (Last 5 Years)",
    subtitle = "Zoomed view to better show the validation period",
    y = "Price ($)",
    x = "Year",
    color = "Dataset"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11),
    axis.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    legend.title = element_text(face = "bold")
  ) +
  # Add a vertical line at the split point
  geom_vline(xintercept = as.numeric(validation_start_date), 
             linetype = "dashed", color = "darkgray")

# Display the zoomed plot
print(zoomed_plot)

# Save the zoomed visualization
ggsave("visuals/2_data_splitting/train_validation_split_zoomed.png", zoomed_plot, width = 10, height = 6, dpi = 300, bg = "white")

# ==============================================================================
# 3. SAVE OBJECTS FOR MODEL BUILDING
# ==============================================================================
# Save the split datasets in the R session's global environment
# This makes them available for subsequent scripts

# Save a message indicating successful completion
cat("\n==============================================================================\n")
cat("DATA SPLITTING COMPLETED\n")
cat("==============================================================================\n")
cat("Data has been split into training and validation sets.\n")
cat("The following objects are available for modeling:\n")
cat("1. Data frames: 'train_df' and 'validation_df'\n")
cat("2. Time series objects: 'train_ts' and 'validation_ts'\n")
cat("3. Visualizations saved in 'visuals/2_data_splitting/' directory\n\n")
cat("Ready to proceed with model building.\n")
cat("==============================================================================\n")

# ==============================================================================
# 4. COMPREHENSIVE PERIOD VISUALIZATION
# ==============================================================================
# Create a visualization showing training, validation, and forecast periods

# Define consistent colors
period_colors <- c(
  "Training" = "#3498DB",    # Light blue
  "Validation" = "#E74C3C",  # Red
  "Forecast" = "#9B59B6"     # Purple
)

# Calculate the forecast period dates
forecast_start_date <- end_date + lubridate::days(1)
forecast_end_date <- forecast_start_date + lubridate::years(1) - lubridate::days(1)

# Create a data frame for the forecast period with prediction intervals
forecast_period <- data.frame(
  date = seq.Date(from = forecast_start_date, to = forecast_end_date, by = "month"),
  egg_price = NA,
  lower = NA,  # Lower prediction interval
  upper = NA,  # Upper prediction interval
  dataset = "Forecast"
)

# Set some reasonable prediction intervals based on historical volatility
last_price <- tail(eggs$egg_price, 1)
forecast_period$egg_price <- last_price  # Use last known price as baseline
forecast_period$lower <- last_price * 0.8  # 20% below
forecast_period$upper <- last_price * 1.2  # 20% above

# Combine all periods
comprehensive_data <- viz_data %>%
  bind_rows(forecast_period)

# Create the comprehensive visualization
periods_plot <- ggplot() +
  # Add lines for each period
  geom_line(data = subset(comprehensive_data, dataset == "Training"),
            aes(x = date, y = egg_price, color = "Training"),
            linewidth = 1) +
  geom_line(data = subset(comprehensive_data, dataset == "Validation"),
            aes(x = date, y = egg_price, color = "Validation"),
            linewidth = 1) +
  # Add forecast period with prediction interval
  geom_line(data = subset(comprehensive_data, dataset == "Forecast"),
            aes(x = date, y = egg_price, color = "Forecast"),
            linewidth = 1,
            linetype = "dotted") +
  geom_ribbon(data = subset(comprehensive_data, dataset == "Forecast"),
              aes(x = date, ymin = lower, ymax = upper),
              fill = period_colors["Forecast"],
              alpha = 0.1) +
  scale_color_manual(
    values = period_colors,
    labels = c("Training Set", "Validation Set", "Forecast Period"),
    breaks = c("Training", "Validation", "Forecast")
  ) +
  # Add vertical lines for period boundaries
  geom_vline(xintercept = as.numeric(validation_start_date), 
             linetype = "dashed", color = "darkgray", alpha = 0.5) +
  geom_vline(xintercept = as.numeric(forecast_start_date), 
             linetype = "dashed", color = "darkgray", alpha = 0.5) +
  labs(
    title = "Comprehensive View of Time Series Periods",
    subtitle = "Training: Jan 1995 to Jan 2024 | Validation: Feb 2024 to Jan 2025 | Forecast: Feb 2025 to Jan 2026",
    y = "Price ($)",
    x = "Year",
    color = "Period"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11),
    axis.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    plot.margin = margin(t = 20, r = 20, b = 10, l = 10, unit = "pt"),
    legend.text = element_text(color = "black"),
    legend.key.width = unit(2, "cm")
  ) +
  guides(color = guide_legend(override.aes = list(
    linetype = c("solid", "solid", "dotted"),  # Match line styles
    linewidth = 1
  )))

# Display the comprehensive plot
print(periods_plot)

# Save the comprehensive visualization
ggsave("visuals/2_data_splitting/comprehensive_periods.png", 
       periods_plot, 
       width = 12, 
       height = 7, 
       dpi = 300, 
       bg = "white")

# Update completion message
cat("\nAdditional visualization created:\n")
cat("4. Comprehensive period visualization saved as 'comprehensive_periods.png'\n")
cat("==============================================================================\n") 
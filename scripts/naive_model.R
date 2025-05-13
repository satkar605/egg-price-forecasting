# ==============================================================================
# NAIVE FORECASTING MODEL
# ==============================================================================
# This script implements a naive forecasting model as a benchmark

# Set working directory to project root
setwd("/Users/satkarkarki/Desktop/Data_Analytics_Portfolio/eggs_forecasting")

# Load necessary libraries
library(tidyverse)    # For data manipulation and visualization
library(forecast)     # For forecasting functions
library(ggplot2)      # For creating visualizations
library(cowplot)      # For arranging multiple plots

# Source the data splitting script to get train/validation datasets
source("scripts/data_splitting.R")

# Confirm data is loaded correctly
cat("Training data dimensions:", dim(train_df)[1], "rows,", dim(train_df)[2], "columns\n")
cat("Validation data dimensions:", dim(validation_df)[1], "rows,", dim(validation_df)[2], "columns\n")
cat("Time series training period:", tsp(train_ts)[1], "to", tsp(train_ts)[2], "\n")
cat("Time series validation period:", tsp(validation_ts)[1], "to", tsp(validation_ts)[2], "\n")

# Create directories for outputs
dir.create("visuals/3_naive_viz", showWarnings = FALSE, recursive = TRUE)
dir.create("data/model_metrics", showWarnings = FALSE, recursive = TRUE)

# ==============================================================================
# 2. MODEL IMPLEMENTATION
# ==============================================================================
# The naive model simply uses the last observed value as the forecast for all future periods
cat("\nFitting naive model to training data...\n")

# Fit naive model to training data
naive_model <- naive(train_ts, h = length(validation_ts))

# Basic information about the model
cat("\nNaive Model Summary:\n")
print(summary(naive_model))

# Display the forecast values
cat("\nForecast values for validation period:\n")
print(naive_model$mean)

# ==============================================================================
# 3. MODEL EVALUATION - TRAINING PERIOD (DATA FIT)
# ==============================================================================
cat("\n==============================================================================\n")
cat("MODEL EVALUATION - TRAINING PERIOD (DATA FIT)\n")
cat("==============================================================================\n")

# Generate fitted values for the training period
# For naive model, the fitted values are just the lagged values (one-step forecasts)
train_fitted <- c(NA, train_ts[-length(train_ts)])

# Calculate training errors
train_errors <- train_ts - train_fitted
train_errors <- train_errors[-1]  # Remove the first NA value
train_actuals <- train_ts[-1]     # Remove the first value to match dimensions

# Calculate performance metrics for training period
train_metrics <- data.frame(
  Metric = c("Average Error", "Mean Absolute Error (MAE)", "Root Mean Square Error (RMSE)", 
             "Mean Absolute Percentage Error (MAPE)"),
  Value = c(
    mean(train_errors, na.rm = TRUE),
    mean(abs(train_errors), na.rm = TRUE),
    sqrt(mean(train_errors^2, na.rm = TRUE)),
    mean(abs(train_errors/train_actuals)*100, na.rm = TRUE)
  )
)

# Display training metrics
cat("\nTraining Period Performance Metrics:\n")
print(train_metrics)

# ==============================================================================
# 4. MODEL EVALUATION - VALIDATION PERIOD (PREDICTIVE ACCURACY)
# ==============================================================================
cat("\n==============================================================================\n")
cat("MODEL EVALUATION - VALIDATION PERIOD (PREDICTIVE ACCURACY)\n")
cat("==============================================================================\n")

# Get the forecasted values for the validation period
valid_forecasts <- naive_model$mean

# Calculate validation errors
valid_errors <- validation_ts - valid_forecasts

# Calculate performance metrics for validation period
valid_metrics <- data.frame(
  Metric = c("Average Error", "Mean Absolute Error (MAE)", "Root Mean Square Error (RMSE)", 
             "Mean Absolute Percentage Error (MAPE)"),
  Value = c(
    mean(valid_errors, na.rm = TRUE),
    mean(abs(valid_errors), na.rm = TRUE),
    sqrt(mean(valid_errors^2, na.rm = TRUE)),
    mean(abs(valid_errors/validation_ts)*100, na.rm = TRUE)
  )
)

# Display validation metrics
cat("\nValidation Period Performance Metrics:\n")
print(valid_metrics)

# Create a summary table comparing training and validation metrics
comparison_metrics <- data.frame(
  Metric = train_metrics$Metric,
  Training = train_metrics$Value,
  Validation = valid_metrics$Value
)

cat("\nComparison of Training vs. Validation Performance:\n")
print(comparison_metrics)

# ==============================================================================
# 5. PERFORMANCE VISUALIZATION
# ==============================================================================
cat("\n==============================================================================\n")
cat("CREATING PERFORMANCE VISUALIZATIONS\n")
cat("==============================================================================\n")

# Create a time series plot of actuals vs. forecasts for validation period
# First, create data frames for plotting
validation_dates <- time(validation_ts)

validation_plot_data <- data.frame(
  Date = as.Date(paste(floor(validation_dates), 
                        round((validation_dates %% 1) * 12) + 1, 
                        "01", sep = "-")),
  Actual = as.numeric(validation_ts),
  Forecast = as.numeric(valid_forecasts),
  Error = as.numeric(valid_errors)
)

# Calculate the range for errors to scale them appropriately
max_price <- max(max(validation_plot_data$Actual), max(validation_plot_data$Forecast))
error_scale_factor <- max_price / max(abs(validation_plot_data$Error)) * 0.5  # Scale errors to be half the size of prices

# Create a single combined plot with actual vs. forecasts and overlaid errors
combined_performance_plot <- ggplot(validation_plot_data, aes(x = Date)) +
  # Add errors as bars
  geom_bar(aes(y = Error * error_scale_factor, fill = "Error"), stat = "identity", alpha = 0.5, width = 15) +
  # Add actual and forecast lines
  geom_line(aes(y = Actual, color = "Actual"), size = 1) +
  geom_line(aes(y = Forecast, color = "Forecast"), size = 1, linetype = "dashed") +
  # Add a horizontal line at zero for errors
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkgray") +
  # Add primary y-axis (for actual and forecast values)
  scale_y_continuous(
    name = "Egg Price ($)",
    # Add secondary y-axis (for errors)
    sec.axis = sec_axis(~./error_scale_factor, name = "Forecast Error")
  ) +
  # Set colors and fills
  scale_color_manual(values = c("Actual" = "#2C3E50", "Forecast" = "#E74C3C")) +
  scale_fill_manual(values = c("Error" = "#3498DB")) +
  # Add titles and labels
  labs(
    title = "Validation Period: Model Performance",
    subtitle = paste0("Naive Model with Overlaid Forecast Errors (", 
                     format(min(validation_plot_data$Date), "%b %Y"), 
                     " to ", format(max(validation_plot_data$Date), "%b %Y"), ")"),
    x = "Month",
    color = "Series",
    fill = "Component"
  ) +
  # Set theme
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11),
    axis.title = element_text(face = "bold"),
    legend.position = "bottom",
    legend.box = "horizontal",
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )

# Display the combined plot
print(combined_performance_plot)

# Save the combined plot
ggsave("visuals/3_naive_viz/performance_combined.png", combined_performance_plot, width = 12, height = 8, dpi = 300, bg = "white")
cat("Combined performance visualization saved to 'visuals/3_naive_viz/performance_combined.png'\n")

# We'll also keep the original separate plots and combined vertical layout for reference
# Create actuals vs. forecasts plot for validation period
valid_actuals_forecasts_plot <- ggplot(validation_plot_data, aes(x = Date)) +
  geom_line(aes(y = Actual, color = "Actual"), size = 1) +
  geom_line(aes(y = Forecast, color = "Forecast"), size = 1, linetype = "dashed") +
  scale_color_manual(values = c("Actual" = "#2C3E50", "Forecast" = "#E74C3C")) +
  labs(
    title = "Validation Period: Actual vs. Forecast",
    subtitle = paste0("Naive Model (", format(min(validation_plot_data$Date), "%b %Y"), 
                     " to ", format(max(validation_plot_data$Date), "%b %Y"), ")"),
    x = "Month",
    y = "Egg Price ($)",
    color = "Series"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11),
    axis.title = element_text(face = "bold"),
    legend.position = "bottom",
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )

# Create errors plot for validation period
valid_errors_plot <- ggplot(validation_plot_data, aes(x = Date, y = Error)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkgray") +
  geom_line(color = "#3498DB", size = 1) +
  geom_point(color = "#3498DB", size = 2) +
  labs(
    title = "Validation Period: Forecast Errors",
    subtitle = "Positive values indicate under-forecasting, negative values indicate over-forecasting",
    x = "Month",
    y = "Error (Actual - Forecast)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11),
    axis.title = element_text(face = "bold"),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )

# Create a combined plot for validation period (vertical arrangement)
validation_combined_plot <- plot_grid(
  valid_actuals_forecasts_plot, 
  valid_errors_plot, 
  nrow = 2, 
  align = "v"
)

# Save the plots
ggsave("visuals/3_naive_viz/validation_comparison.png", valid_actuals_forecasts_plot, width = 10, height = 6, dpi = 300, bg = "white")
ggsave("visuals/3_naive_viz/validation_errors.png", valid_errors_plot, width = 10, height = 4, dpi = 300, bg = "white")
ggsave("visuals/3_naive_viz/validation_combined.png", validation_combined_plot, width = 10, height = 10, dpi = 300, bg = "white")

# ==============================================================================
# 6. FITTING FINAL MODEL ON FULL DATASET
# ==============================================================================
cat("\n==============================================================================\n")
cat("FITTING FINAL MODEL ON FULL DATASET\n")
cat("==============================================================================\n")

# Now that we've evaluated the model, we can fit it on the full dataset
# for future forecasting
full_naive_model <- naive(eggs_ts, h = 12)  # Forecast 12 months ahead

# Summary of the final model
cat("\nFinal Naive Model Summary (Fitted on Full Dataset):\n")
print(summary(full_naive_model))

# Visualization of the full forecast
future_dates <- seq(max(time(eggs_ts)) + 1/12, 
                   by = 1/12, length.out = 12)

future_dates_df <- data.frame(
  Date = as.Date(paste(floor(future_dates), 
                       round((future_dates %% 1) * 12) + 1, 
                       "01", sep = "-")),
  Forecast = as.numeric(full_naive_model$mean),
  Lower_80 = as.numeric(full_naive_model$lower[,1]),
  Upper_80 = as.numeric(full_naive_model$upper[,1]),
  Lower_95 = as.numeric(full_naive_model$lower[,2]),
  Upper_95 = as.numeric(full_naive_model$upper[,2])
)

# Create historical dates dataframe
historical_dates <- time(eggs_ts)
historical_df <- data.frame(
  Date = as.Date(paste(floor(historical_dates), 
                       round((historical_dates %% 1) * 12) + 1, 
                       "01", sep = "-")),
  Value = as.numeric(eggs_ts)
)

# Create full forecast plot
full_forecast_plot <- ggplot() +
  # Historical data
  geom_line(data = historical_df, aes(x = Date, y = Value, color = "Historical"), size = 1) +
  # Forecast
  geom_line(data = future_dates_df, aes(x = Date, y = Forecast, color = "Forecast"), size = 1) +
  # 95% Prediction Interval
  geom_ribbon(data = future_dates_df, 
              aes(x = Date, ymin = Lower_95, ymax = Upper_95), 
              fill = "#E74C3C", alpha = 0.2) +
  # 80% Prediction Interval
  geom_ribbon(data = future_dates_df, 
              aes(x = Date, ymin = Lower_80, ymax = Upper_80), 
              fill = "#E74C3C", alpha = 0.3) +
  # Colors and labels
  scale_color_manual(values = c("Historical" = "#2C3E50", "Forecast" = "#E74C3C")) +
  labs(
    title = "Naive Forecast: 12-Month Horizon",
    subtitle = "With 80% and 95% prediction intervals",
    x = "Month",
    y = "Egg Price ($)",
    color = "Series"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11),
    axis.title = element_text(face = "bold"),
    legend.position = "bottom",
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )

# Display the plot
print(full_forecast_plot)

# Save the forecast plot
ggsave("visuals/3_naive_viz/full_forecast.png", full_forecast_plot, width = 10, height = 6, dpi = 300, bg = "white")

# Save model results to file
model_results <- list(
  train_metrics = train_metrics,
  valid_metrics = valid_metrics,
  comparison = comparison_metrics,
  validation_forecasts = data.frame(
    Date = validation_plot_data$Date,
    Actual = validation_plot_data$Actual,
    Forecast = validation_plot_data$Forecast,
    Error = validation_plot_data$Error
  ),
  future_forecasts = future_dates_df
)

# Serialize the model results
saveRDS(model_results, "data/model_metrics/naive_model_results.rds")

# ==============================================================================
# SUMMARY
# ==============================================================================
cat("\n==============================================================================\n")
cat("NAIVE MODEL EVALUATION COMPLETED\n")
cat("==============================================================================\n")
cat("Key findings:\n")
cat("1. Training RMSE:", round(train_metrics$Value[3], 3), "\n")
cat("2. Validation RMSE:", round(valid_metrics$Value[3], 3), "\n")
cat("3. Validation MAPE:", round(valid_metrics$Value[4], 3), "%\n")
cat("4. Model performance visualizations saved to the 'visuals/3_naive_viz/' directory\n")
cat("5. 12-month forecast generated with prediction intervals\n")
cat("==============================================================================\n")

# ==============================================================================
# CREATE FORMATTED PERFORMANCE TABLES
# ==============================================================================
cat("\n==============================================================================\n")
cat("MODEL PERFORMANCE SUMMARY TABLES\n")
cat("==============================================================================\n")

# Calculate additional metrics for formatted tables
train_sse <- sum(train_errors^2, na.rm = TRUE)
valid_sse <- sum(valid_errors^2, na.rm = TRUE)

train_rmse <- sqrt(mean(train_errors^2, na.rm = TRUE))
valid_rmse <- sqrt(mean(valid_errors^2, na.rm = TRUE))

train_avg_error <- mean(train_errors, na.rm = TRUE)
valid_avg_error <- mean(valid_errors, na.rm = TRUE)

# Create training performance table data
training_table <- data.frame(
  Metric = c("Total sum of squared errors", "RMS Error", "Average Error"),
  Value = c(train_sse, train_rmse, train_avg_error)
)

# Create validation performance table data
validation_table <- data.frame(
  Metric = c("Total sum of squared errors", "RMS Error", "Average Error"),
  Value = c(valid_sse, valid_rmse, valid_avg_error)
)

# Display the performance tables
cat("\nTraining Data scoring - Summary Report (Data Fit)\n")
cat("--------------------------------------------------\n")
cat(sprintf("%-25s | %-15s | %-15s\n", "Metric", "Value", ""))
cat("--------------------------------------------------\n")
cat(sprintf("%-25s | %-15.2f | %-15s\n", "Total sum of squared errors", train_sse, ""))
cat(sprintf("%-25s | %-15.2f | %-15s\n", "RMS Error", train_rmse, ""))
cat(sprintf("%-25s | %-15.6f | %-15s\n", "Average Error", train_avg_error, ""))
cat("--------------------------------------------------\n\n")

cat("\nValidation Data scoring - Summary Report (Predictive Accuracy)\n")
cat("--------------------------------------------------\n")
cat(sprintf("%-25s | %-15s | %-15s\n", "Metric", "Value", ""))
cat("--------------------------------------------------\n")
cat(sprintf("%-25s | %-15.2f | %-15s\n", "Total sum of squared errors", valid_sse, ""))
cat(sprintf("%-25s | %-15.2f | %-15s\n", "RMS Error", valid_rmse, ""))
cat(sprintf("%-25s | %-15.6f | %-15s\n", "Average Error", valid_avg_error, ""))
cat("--------------------------------------------------\n\n")

# Create a more visually appealing performance summary table image
if (requireNamespace("gridExtra", quietly = TRUE) && requireNamespace("grid", quietly = TRUE)) {
  library(gridExtra)
  library(grid)
  
  # Function to create a nice looking table
  create_table_grob <- function(data, title) {
    # Format the numbers nicely
    data$Value <- format(round(data$Value, 4), big.mark = ",")
    
    # Create the table
    table_grob <- gridExtra::tableGrob(
      data, 
      rows = NULL,
      theme = gridExtra::ttheme_minimal(
        core = list(fg_params = list(fontface = "plain"),
                   bg_params = list(fill = c("#E6E6FA", "#D8D8D8"))),
        colhead = list(fg_params = list(fontface = "bold"),
                      bg_params = list(fill = "#B0C4DE"))
      )
    )
    
    # Add a title
    title_grob <- grid::textGrob(
      title, 
      gp = grid::gpar(fontface = "bold", fontsize = 14, col = "#4B0082")
    )
    
    # Combine title and table
    result <- gridExtra::arrangeGrob(
      title_grob,
      table_grob,
      heights = c(0.2, 0.8)
    )
    
    return(result)
  }
  
  # Create the tables
  train_title <- "Training Data scoring - Summary Report"
  valid_title <- "Validation Data scoring - Summary Report"
  
  train_table_grob <- create_table_grob(training_table, train_title)
  valid_table_grob <- create_table_grob(validation_table, valid_title)
  
  # Combine tables
  performance_tables <- gridExtra::arrangeGrob(
    train_table_grob, 
    valid_table_grob,
    ncol = 1
  )
  
  # Add "Model Performance" title
  model_title <- grid::textGrob(
    "Model Performance: Naive Model", 
    gp = grid::gpar(fontface = "bold", fontsize = 24, col = "#8B0000")
  )
  
  # Add "Data Fit" and "Predictive Accuracy" labels directly in the tables instead of as separate elements
  # Create a better layout without labels that are too far to the right
  final_layout <- gridExtra::grid.arrange(
    model_title,
    train_table_grob,
    grid::textGrob("Data Fit", gp = grid::gpar(fontface = "bold", fontsize = 18)),
    valid_table_grob,
    grid::textGrob("Predictive Accuracy", gp = grid::gpar(fontface = "bold", fontsize = 18)),
    layout_matrix = rbind(
      c(1, 1),
      c(2, 3),
      c(4, 5)
    ),
    heights = c(0.15, 0.4, 0.4)
  )
  
  # Save the performance tables
  ggsave("visuals/3_naive_viz/performance_tables.png", final_layout, width = 12, height = 8, dpi = 300, bg = "white")
  cat("Model performance summary tables saved to 'visuals/3_naive_viz/performance_tables.png'\n")
} else {
  cat("Note: gridExtra and grid packages are required to create formatted performance tables.\n")
  cat("Install them with: install.packages(c('gridExtra', 'grid'))\n")
}

# Save Naive model performance metrics to CSV
naive_performance_comparison <- data.frame(
  Period = c("Training", "Training", "Training", "Training", "Validation", "Validation", "Validation", "Validation"),
  Metric = c("Mean Error", "Mean Absolute Error", "Root Mean Square Error", "Mean Absolute Percentage Error",
             "Mean Error", "Mean Absolute Error", "Root Mean Square Error", "Mean Absolute Percentage Error"),
  Value = c(
    mean(train_errors, na.rm = TRUE),
    mean(abs(train_errors), na.rm = TRUE),
    sqrt(mean(train_errors^2, na.rm = TRUE)),
    mean(abs(train_errors/train_actuals)*100, na.rm = TRUE),
    mean(valid_errors, na.rm = TRUE),
    mean(abs(valid_errors), na.rm = TRUE),
    sqrt(mean(valid_errors^2, na.rm = TRUE)),
    mean(abs(valid_errors/validation_ts)*100, na.rm = TRUE)
  ),
  Unit = c("$", "$", "$", "%", "$", "$", "$", "%")
)

write.csv(naive_performance_comparison, "data/model_metrics/naive_performance_comparison.csv", row.names = FALSE)
cat("Naive model performance metrics saved to 'data/model_metrics/naive_performance_comparison.csv'\n")
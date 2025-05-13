# Load necessary libraries
library(forecast)
library(ggplot2)
library(gridExtra)
library(grid)

# Ensure proper output directories exist
dir.create("visuals/5_arima_viz", showWarnings = FALSE, recursive = TRUE)
dir.create("data/model_metrics", showWarnings = FALSE, recursive = TRUE)

# Load the data
source('scripts/load_data.R')

# Split the data into training and validation sets
source('scripts/data_splitting.R')

# ==============================================================================
# ARIMA MODEL SELECTION
# ==============================================================================
cat("\n==============================================================================\n")
cat("ARIMA MODEL SELECTION\n")
cat("==============================================================================\n")

# Use auto.arima to select the best ARIMA model
arima_model <- auto.arima(train_ts)
cat("\nSelected ARIMA Model:\n")
print(summary(arima_model))

# ==============================================================================
# MODEL EVALUATION
# ==============================================================================
cat("\n==============================================================================\n")
cat("MODEL EVALUATION\n")
cat("==============================================================================\n")

# Forecast on training data
tf_train <- fitted(arima_model)
train_errors <- train_ts - tf_train

# Forecast on validation data
forecast_valid <- forecast(arima_model, h = length(validation_ts))
validation_errors <- validation_ts - forecast_valid$mean

# Calculate performance metrics
train_sse <- sum(train_errors^2, na.rm = TRUE)
valid_sse <- sum(validation_errors^2, na.rm = TRUE)

train_rmse <- sqrt(mean(train_errors^2, na.rm = TRUE))
valid_rmse <- sqrt(mean(validation_errors^2, na.rm = TRUE))

train_avg_error <- mean(train_errors, na.rm = TRUE)
valid_avg_error <- mean(validation_errors, na.rm = TRUE)

# Create performance tables
training_table <- data.frame(
  Metric = c("Total sum of squared errors", "RMS Error", "Average Error"),
  Value = c(train_sse, train_rmse, train_avg_error)
)

validation_table <- data.frame(
  Metric = c("Total sum of squared errors", "RMS Error", "Average Error"),
  Value = c(valid_sse, valid_rmse, valid_avg_error)
)

# Display performance tables
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

# Save ARIMA model performance metrics to CSV
arima_performance_comparison <- data.frame(
  Period = c("Training", "Training", "Training", "Training", "Validation", "Validation", "Validation", "Validation"),
  Metric = c("Mean Error", "Mean Absolute Error", "Root Mean Square Error", "Mean Absolute Percentage Error",
             "Mean Error", "Mean Absolute Error", "Root Mean Square Error", "Mean Absolute Percentage Error"),
  Value = c(
    train_avg_error,
    mean(abs(train_errors), na.rm = TRUE),
    train_rmse,
    mean(abs(train_errors/train_ts)*100, na.rm = TRUE),
    valid_avg_error,
    mean(abs(validation_errors), na.rm = TRUE),
    valid_rmse,
    mean(abs(validation_errors/validation_ts)*100, na.rm = TRUE)
  ),
  Unit = c("$", "$", "$", "%", "$", "$", "$", "%")
)

write.csv(arima_performance_comparison, "data/model_metrics/arima_performance_comparison.csv", row.names = FALSE)
cat("ARIMA model performance metrics saved to 'data/model_metrics/arima_performance_comparison.csv'\n")

# ==============================================================================
# PERFORMANCE CHARTS
# ==============================================================================
cat("\n==============================================================================\n")
cat("CREATING PERFORMANCE CHARTS\n")
cat("==============================================================================\n")

# Create performance visualization
if (requireNamespace("gridExtra", quietly = TRUE) && requireNamespace("grid", quietly = TRUE)) {
  library(gridExtra)
  library(grid)
  
  create_table_grob <- function(data, title) {
    data$Value <- format(round(data$Value, 4), big.mark = ",")
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
    title_grob <- grid::textGrob(
      title, 
      gp = grid::gpar(fontface = "bold", fontsize = 14, col = "#4B0082")
    )
    result <- gridExtra::arrangeGrob(
      title_grob,
      table_grob,
      heights = c(0.2, 0.8)
    )
    return(result)
  }
  
  train_title <- "Training Data scoring - Summary Report"
  valid_title <- "Validation Data scoring - Summary Report"
  
  train_table_grob <- create_table_grob(training_table, train_title)
  valid_table_grob <- create_table_grob(validation_table, valid_title)
  
  model_title <- grid::textGrob(
    "Model Performance: ARIMA Model", 
    gp = grid::gpar(fontface = "bold", fontsize = 24, col = "#8B0000")
  )
  
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
  
  print(final_layout)
  ggsave("visuals/5_arima_viz/performance_tables.png", final_layout, width = 12, height = 8, dpi = 300, bg = "white")
  cat("Model performance summary tables saved to 'visuals/5_arima_viz/performance_tables.png'\n")
} else {
  cat("Note: gridExtra and grid packages are required to create formatted performance tables.\n")
  cat("Install them with: install.packages(c('gridExtra', 'grid'))\n")
}

# ==============================================================================
# VISUAL COMPARISON TABLE
# ==============================================================================
cat("\n==============================================================================\n")
cat("CREATING VISUAL COMPARISON TABLE\n")
cat("==============================================================================\n")

comparison_data <- data.frame(
  Model = c("Naive", "ETS", "ARIMA"),
  Training_RMSE = c(0.1871, 0.1871, train_rmse),
  Validation_RMSE = c(1.0169, 0.9411, valid_rmse)
)

comparison_grob <- gridExtra::tableGrob(
  comparison_data, 
  rows = NULL,
  theme = gridExtra::ttheme_minimal(
    core = list(fg_params = list(fontface = "plain"),
               bg_params = list(fill = c("#E6E6FA", "#D8D8D8"))),
    colhead = list(fg_params = list(fontface = "bold"),
                  bg_params = list(fill = "#B0C4DE"))
  )
)

comparison_title <- grid::textGrob(
  "Model Comparison: RMSE", 
  gp = grid::gpar(fontface = "bold", fontsize = 24, col = "#8B0000")
)

comparison_layout <- gridExtra::arrangeGrob(
  comparison_title,
  comparison_grob,
  heights = c(0.2, 0.8)
)

print(comparison_layout)
ggsave("visuals/5_arima_viz/model_comparison_visual.png", comparison_layout, width = 10, height = 6, dpi = 300, bg = "white")
cat("Visual comparison table saved to 'visuals/5_arima_viz/model_comparison_visual.png'\n")

# ==============================================================================
# ACTUAL VS FORECAST AND VALIDATION ERROR CHARTS
# ==============================================================================
cat("\n==============================================================================\n")
cat("CREATING ACTUAL VS FORECAST AND VALIDATION ERROR CHARTS\n")
cat("==============================================================================\n")

# Actual vs Forecast plot
actual_vs_forecast_plot <- ggplot() +
  geom_line(aes(x = time(validation_ts), y = validation_ts, color = "Actual"), size = 1) +
  geom_line(aes(x = time(validation_ts), y = forecast_valid$mean, color = "Forecast"), linetype = "dashed", size = 1) +
  labs(
    title = "Actual vs Forecast",
    x = "Time",
    y = "Egg Price",
    color = "Legend"
  ) +
  scale_color_manual(values = c("Actual" = "blue", "Forecast" = "red")) +
  theme_minimal()

# Validation Error plot
validation_error_plot <- ggplot() +
  geom_line(aes(x = time(validation_ts), y = validation_errors), color = "purple", size = 1) +
  labs(
    title = "Validation Errors",
    x = "Time",
    y = "Error"
  ) +
  theme_minimal()

# Combine plots
combined_plot <- gridExtra::grid.arrange(
  actual_vs_forecast_plot,
  validation_error_plot,
  ncol = 1
)

# Display the combined plot
print(combined_plot)

ggsave("visuals/5_arima_viz/combined_performance.png", combined_plot, width = 10, height = 12, dpi = 300, bg = "white")
cat("Combined Actual vs Forecast and Validation Error charts saved to 'visuals/5_arima_viz/' directory\n")

# ==============================================================================
# READY FOR FORECASTING
# ==============================================================================
cat("\nReady to proceed with forecasting.\n") 
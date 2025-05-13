# ==============================================================================
# ETS MODEL IMPLEMENTATION FOR EGG PRICE FORECASTING
# ==============================================================================
# This script implements an ETS (Error, Trend, Seasonality) model for egg price
# forecasting, evaluating different model specifications and parameters

# Set working directory to project root
setwd("/Users/satkarkarki/Desktop/Data_Analytics_Portfolio/eggs_forecasting")

# Load necessary libraries
library(tidyverse)    # For data manipulation and visualization
library(forecast)     # For ETS modeling and forecasting functions
library(cowplot)      # For arranging multiple plots

# Create models directory if it doesn't exist
dir.create("visuals/4_ets_viz", recursive = TRUE, showWarnings = FALSE)

# Source the data splitting script to get train/validation datasets
source("scripts/data_splitting.R")

# Confirm data is loaded correctly
cat("Training data dimensions:", dim(train_df)[1], "rows,", dim(train_df)[2], "columns\n")
cat("Validation data dimensions:", dim(validation_df)[1], "rows,", dim(validation_df)[2], "columns\n")
cat("Time series training period:", tsp(train_ts)[1], "to", tsp(train_ts)[2], "\n")
cat("Time series validation period:", tsp(validation_ts)[1], "to", tsp(validation_ts)[2], "\n")

# ==============================================================================
# 1. EXPLORING ETS MODELS
# ==============================================================================
cat("\n==============================================================================\n")
cat("EXPLORING DIFFERENT ETS MODEL SPECIFICATIONS\n")
cat("==============================================================================\n")

# Function to evaluate an ETS model with specified parameters
evaluate_ets_model <- function(model_spec, alpha_value = NULL) {
  # If alpha is provided, fit model with specified alpha
  if (!is.null(alpha_value)) {
    ets_model <- ets(train_ts, model = model_spec, alpha = alpha_value)
  } else {
    # Otherwise, let the function determine optimal parameters
    ets_model <- ets(train_ts, model = model_spec)
  }
  
  # Generate forecasts
  ets_forecast <- forecast(ets_model, h = length(validation_ts))
  
  # Calculate performance metrics
  forecast_errors <- validation_ts - ets_forecast$mean
  rmse <- sqrt(mean(forecast_errors^2))
  mae <- mean(abs(forecast_errors))
  mape <- mean(abs(forecast_errors/validation_ts) * 100)
  
  return(list(
    model = ets_model, 
    forecast = ets_forecast, 
    rmse = rmse, 
    mae = mae, 
    mape = mape,
    errors = forecast_errors
  ))
}

# Test different ETS model specifications
cat("\nTesting different ETS model specifications:\n")
model_specs <- c("ANN", "AAN", "AAA", "MNN", "MAN", "MAA")

# Create a list to store all results
model_results <- list()

# Evaluate each model specification
for (model_spec in model_specs) {
  cat("\nFitting ETS model with specification:", model_spec, "\n")
  model_results[[model_spec]] <- evaluate_ets_model(model_spec)
  cat("Model information:\n")
  print(model_results[[model_spec]]$model)
  cat("RMSE:", model_results[[model_spec]]$rmse, "\n")
  cat("MAE:", model_results[[model_spec]]$mae, "\n")
  cat("MAPE:", model_results[[model_spec]]$mape, "%\n")
}

# Compare performance across model specifications
model_comparison <- data.frame(
  Model = names(model_results),
  RMSE = sapply(model_results, function(x) x$rmse),
  MAE = sapply(model_results, function(x) x$mae),
  MAPE = sapply(model_results, function(x) x$mape)
)

# Order by RMSE
model_comparison <- model_comparison[order(model_comparison$RMSE), ]

cat("\nModel Comparison (ordered by RMSE):\n")
print(model_comparison)

# Add visualization for model comparison
model_comparison_long <- model_comparison %>%
  pivot_longer(cols = c(RMSE, MAE, MAPE), 
               names_to = "Metric", 
               values_to = "Value")

# Create separate plots for RMSE/MAE and MAPE (due to scale differences)
model_rmse_mae_plot <- model_comparison_long %>%
  filter(Metric != "MAPE") %>%
  ggplot(aes(x = reorder(Model, -Value), y = Value, fill = Metric)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("RMSE" = "#E74C3C", "MAE" = "#3498DB")) +
  labs(
    title = "ETS Model Comparison - RMSE and MAE",
    subtitle = "Lower values indicate better performance",
    x = "Model Specification",
    y = "Error Value"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 12),
    axis.title = element_text(face = "bold"),
    legend.position = "top"
  )

model_mape_plot <- model_comparison_long %>%
  filter(Metric == "MAPE") %>%
  ggplot(aes(x = reorder(Model, -Value), y = Value, fill = Metric)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("MAPE" = "#2ECC71")) +
  labs(
    title = "ETS Model Comparison - MAPE",
    subtitle = "Lower values indicate better performance",
    x = "Model Specification",
    y = "Percentage Error (%)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 12),
    axis.title = element_text(face = "bold"),
    legend.position = "top"
  )

# Save the model comparison plots
ggsave("visuals/4_ets_viz/ets_model_comparison_rmse_mae.png", model_rmse_mae_plot, width = 10, height = 6, dpi = 300, bg = "white")
ggsave("visuals/4_ets_viz/ets_model_comparison_mape.png", model_mape_plot, width = 10, height = 6, dpi = 300, bg = "white")

# Display the plots
print(model_rmse_mae_plot)
print(model_mape_plot)

# Identify the best model specification
best_spec <- model_comparison$Model[1]
cat("\nBest model specification based on RMSE:", best_spec, "\n")

# ==============================================================================
# 2. OPTIMIZING THE BEST MODEL
# ==============================================================================
cat("\n==============================================================================\n")
cat("OPTIMIZING THE BEST MODEL WITH DIFFERENT SMOOTHING PARAMETERS\n")
cat("==============================================================================\n")

# Test a range of alpha values with the best model specification
alpha_values <- c(0.1, 0.3, 0.5, 0.7, 0.9)
alpha_results <- list()

cat("\nTesting", best_spec, "model with different alpha values:\n")
for (alpha in alpha_values) {
  cat("\nAlpha =", alpha, "\n")
  alpha_results[[as.character(alpha)]] <- evaluate_ets_model(best_spec, alpha)
  cat("RMSE:", alpha_results[[as.character(alpha)]]$rmse, "\n")
  cat("MAE:", alpha_results[[as.character(alpha)]]$mae, "\n")
  cat("MAPE:", alpha_results[[as.character(alpha)]]$mape, "%\n")
}

# Compare performance across alpha values
alpha_comparison <- data.frame(
  Alpha = alpha_values,
  RMSE = sapply(alpha_results, function(x) x$rmse),
  MAE = sapply(alpha_results, function(x) x$mae),
  MAPE = sapply(alpha_results, function(x) x$mape)
)

cat("\nAlpha Parameter Comparison for", best_spec, "model:\n")
print(alpha_comparison)

# Add visualization for alpha comparison
alpha_comparison_long <- alpha_comparison %>%
  pivot_longer(cols = c(RMSE, MAE, MAPE), 
               names_to = "Metric", 
               values_to = "Value")

# Create separate plots for RMSE/MAE and MAPE
alpha_rmse_mae_plot <- alpha_comparison_long %>%
  filter(Metric != "MAPE") %>%
  ggplot(aes(x = Alpha, y = Value, color = Metric, group = Metric)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  scale_color_manual(values = c("RMSE" = "#E74C3C", "MAE" = "#3498DB")) +
  labs(
    title = paste("Alpha Parameter Tuning for", best_spec, "Model - RMSE and MAE"),
    subtitle = "Effect of different alpha values on forecast accuracy",
    x = "Alpha Value",
    y = "Error Value"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 12),
    axis.title = element_text(face = "bold"),
    legend.position = "top"
  )

alpha_mape_plot <- alpha_comparison_long %>%
  filter(Metric == "MAPE") %>%
  ggplot(aes(x = Alpha, y = Value, color = Metric, group = Metric)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  scale_color_manual(values = c("MAPE" = "#2ECC71")) +
  labs(
    title = paste("Alpha Parameter Tuning for", best_spec, "Model - MAPE"),
    subtitle = "Effect of different alpha values on forecast accuracy",
    x = "Alpha Value",
    y = "Percentage Error (%)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 12),
    axis.title = element_text(face = "bold"),
    legend.position = "top"
  )

# Save the alpha comparison plots
ggsave("visuals/4_ets_viz/ets_alpha_comparison_rmse_mae.png", alpha_rmse_mae_plot, width = 10, height = 6, dpi = 300, bg = "white")
ggsave("visuals/4_ets_viz/ets_alpha_comparison_mape.png", alpha_mape_plot, width = 10, height = 6, dpi = 300, bg = "white")

# Display the plots
print(alpha_rmse_mae_plot)
print(alpha_mape_plot)

# Identify the best alpha value
best_alpha_row <- which.min(alpha_comparison$RMSE)
best_alpha <- alpha_comparison$Alpha[best_alpha_row]
cat("\nBest alpha value based on RMSE:", best_alpha, "\n")

# ==============================================================================
# 3. FINAL MODEL FITTING
# ==============================================================================
cat("\n==============================================================================\n")
cat("FINAL MODEL FITTING AND EVALUATION\n")
cat("==============================================================================\n")

# Let's also fit a model with automatic parameter selection for comparison
auto_ets_model <- ets(train_ts, model = "ZZZ")
cat("\nAutomatic ETS Model Selection:\n")
print(summary(auto_ets_model))
cat("\nAuto-selected model:", auto_ets_model$method, "\n")

# Decide on the final model (either best manual or auto-selected)
if (best_spec == substr(auto_ets_model$method, 1, 3)) {
  cat("\nBest manual specification matches auto-selected model type.\n")
  final_model <- auto_ets_model
  model_type <- "auto"
} else {
  cat("\nBest manual specification differs from auto-selected model.\n")
  # Check if auto model performs better
  auto_forecast <- forecast(auto_ets_model, h = length(validation_ts))
  auto_errors <- validation_ts - auto_forecast$mean
  auto_rmse <- sqrt(mean(auto_errors^2))
  
  best_manual_rmse <- model_results[[best_spec]]$rmse
  
  cat("Auto model RMSE:", auto_rmse, "\n")
  cat("Best manual model RMSE:", best_manual_rmse, "\n")
  
  if (auto_rmse < best_manual_rmse) {
    cat("Auto-selected model performs better. Using this as final model.\n")
    final_model <- auto_ets_model
    model_type <- "auto"
  } else {
    cat("Manually selected model performs better. Using", best_spec, "as final model.\n")
    final_model <- model_results[[best_spec]]$model
    model_type <- "manual"
  }
}

# Generate forecasts with the final model
final_forecast <- forecast(final_model, h = length(validation_ts))
final_errors <- validation_ts - final_forecast$mean

# Calculate final performance metrics
final_metrics <- data.frame(
  Metric = c("RMSE", "MAE", "MAPE"),
  Value = c(
    sqrt(mean(final_errors^2)),
    mean(abs(final_errors)),
    mean(abs(final_errors/validation_ts) * 100)
  )
)

cat("\nFinal Model Performance Metrics:\n")
print(final_metrics)

# ==============================================================================
# MODEL EVALUATION - PERFORMANCE COMPARISON
# ==============================================================================
cat("\n==============================================================================\n")
cat("MODEL EVALUATION - PERFORMANCE COMPARISON\n")
cat("==============================================================================\n")

# Calculate training period metrics
train_fitted <- fitted(final_model)
train_errors <- train_ts - train_fitted

# Calculate validation period metrics
validation_errors <- validation_ts - final_forecast$mean

# Calculate additional metrics for formatted tables
train_sse <- sum(train_errors^2, na.rm = TRUE)
valid_sse <- sum(validation_errors^2, na.rm = TRUE)

train_rmse <- sqrt(mean(train_errors^2, na.rm = TRUE))
valid_rmse <- sqrt(mean(validation_errors^2, na.rm = TRUE))

train_avg_error <- mean(train_errors, na.rm = TRUE)
valid_avg_error <- mean(validation_errors, na.rm = TRUE)

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
  
  # Add "Model Performance" title
  model_title <- grid::textGrob(
    paste("Model Performance:", if(model_type == "auto") auto_ets_model$method else best_spec, "Model"), 
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
  
  # Display the performance tables
  print(final_layout)
  
  # Save the performance tables
  ggsave("visuals/4_ets_viz/ets_performance_tables.png", final_layout, width = 12, height = 8, dpi = 300, bg = "white")
  cat("Model performance summary tables saved to visuals/4_ets_viz/ets_performance_tables.png\n")
} else {
  cat("Note: gridExtra and grid packages are required to create formatted performance tables.\n")
  cat("Install them with: install.packages(c('gridExtra', 'grid'))\n")
}

# Create a dataframe for plotting
validation_dates <- time(validation_ts)
validation_years <- floor(validation_dates)
validation_months <- round((validation_dates - validation_years) * 12) + 1
validation_dates_df <- data.frame(
  date = as.Date(paste(validation_years, validation_months, "1", sep = "-")),
  actual = as.numeric(validation_ts),
  forecast = as.numeric(final_forecast$mean),
  lower95 = as.numeric(final_forecast$lower[, 2]),
  upper95 = as.numeric(final_forecast$upper[, 2]),
  error = as.numeric(validation_ts) - as.numeric(final_forecast$mean)
)

# Create combined validation plot with forecasts and errors overlaid
# This approach should be used for all model visualizations
combined_validation_plot <- ggplot(validation_dates_df) +
  # Primary y-axis: Actual and forecast values with CI
  geom_ribbon(aes(x = date, ymin = lower95, ymax = upper95), fill = "lightblue", alpha = 0.3) +
  geom_line(aes(x = date, y = forecast, color = "Forecast"), size = 1) +
  geom_line(aes(x = date, y = actual, color = "Actual"), size = 1) +
  
  # Add a secondary y-axis for errors
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkgray") +
  geom_line(aes(x = date, y = error, color = "Error"), size = 1, linetype = "dotted") +
  geom_point(aes(x = date, y = error, color = "Error"), size = 2) +
  
  # Customize colors and labels
  scale_color_manual(values = c("Actual" = "#2C3E50", "Forecast" = "#E74C3C", "Error" = "#27AE60")) +
  labs(
    title = paste("ETS Model Validation: Actual vs. Forecast with Errors"),
    subtitle = paste("Model:", if(model_type == "auto") auto_ets_model$method else best_spec,
                    "| Mean Error:", round(mean(final_errors), 4)),
    y = "Price ($)",
    x = "Date",
    color = "Series"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 12),
    axis.title = element_text(face = "bold"),
    legend.position = "bottom"
  )

# Display the combined plot
print(combined_validation_plot)

# Save the combined plot
ggsave("visuals/4_ets_viz/ets_combined_validation.png", combined_validation_plot, width = 12, height = 8, dpi = 300, bg = "white")

# Note: We're keeping individual plots for reference but the combined visualization
# should be the primary output for all model evaluations
validation_plot <- ggplot(validation_dates_df, aes(x = date)) +
  geom_ribbon(aes(ymin = lower95, ymax = upper95), fill = "lightblue", alpha = 0.3) +
  geom_line(aes(y = forecast, color = "Forecast"), size = 1) +
  geom_line(aes(y = actual, color = "Actual"), size = 1) +
  scale_color_manual(values = c("Actual" = "#2C3E50", "Forecast" = "#E74C3C")) +
  labs(
    title = paste("ETS Model Forecast vs. Actual Values"),
    subtitle = paste("Model:", if(model_type == "auto") auto_ets_model$method else best_spec),
    y = "Price ($)",
    x = "Date",
    color = ""
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 12),
    axis.title = element_text(face = "bold"),
    legend.position = "bottom"
  )

# Create error plot
error_plot <- ggplot(validation_dates_df, aes(x = date, y = actual - forecast)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkgray") +
  geom_line(color = "#E74C3C") +
  geom_point(color = "#E74C3C") +
  labs(
    title = "Forecast Errors",
    subtitle = paste("Mean Error:", round(mean(final_errors), 4)),
    y = "Error (Actual - Forecast)",
    x = "Date"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 12),
    axis.title = element_text(face = "bold")
  )

# Display the plots
print(validation_plot)
print(error_plot)

# Combine plots for saving
combined_plot <- plot_grid(validation_plot, error_plot, nrow = 2)

# Save the plots
ggsave("visuals/4_ets_viz/ets_forecast_vs_actual.png", validation_plot, width = 10, height = 6, dpi = 300, bg = "white")
ggsave("visuals/4_ets_viz/ets_forecast_errors.png", error_plot, width = 10, height = 4, dpi = 300, bg = "white")
ggsave("visuals/4_ets_viz/ets_combined_results.png", combined_plot, width = 10, height = 10, dpi = 300, bg = "white")

# ==============================================================================
# 5. FUTURE FORECASTING
# ==============================================================================
cat("\n==============================================================================\n")
cat("GENERATING FUTURE FORECASTS\n")
cat("==============================================================================\n")

# Train final model on complete dataset (including validation period)
future_forecast <- NULL  # Initialize to prevent errors
full_model <- NULL       # Initialize to prevent errors

tryCatch({
  if (model_type == "auto") {
    # If we used auto selection before, use it again on full dataset
    full_model <- ets(eggs_ts, model = "ZZZ")
    cat("\nAutomatic model selection on full dataset:\n")
  } else {
    # If we used a specific model, extract its components
    # For MAN model, specify explicitly to avoid "Invalid error type" error
    if (best_spec == "MAN") {
      cat("\nUsing MAN model with explicit parameters\n")
      full_model <- ets(eggs_ts, model = "MAN", opt.crit = "mse", damped = FALSE)
    } else {
      model_spec <- final_model$method
      cat("\nUsing previously selected model specification:", model_spec, "\n")
      full_model <- ets(eggs_ts, model = model_spec)
    }
  }
  
  if (!is.null(full_model)) {
    cat("\nFinal Model Trained on Complete Dataset:\n")
    print(summary(full_model))
    
    # Generate 12-month future forecast
    future_months <- 12
    future_forecast <- forecast(full_model, h = future_months)
    
    # Display forecast
    cat("\nForecast for next", future_months, "months:\n")
    print(future_forecast)
    
    # Create forecast plot
    future_plot <- autoplot(future_forecast) +
      labs(
        title = "ETS Model: 12-Month Egg Price Forecast",
        subtitle = paste("Model:", full_model$method),
        y = "Price ($)",
        x = "Year"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(size = 12),
        axis.title = element_text(face = "bold")
      )
    
    # Display and save the plot
    print(future_plot)
    ggsave("visuals/4_ets_viz/ets_future_forecast.png", future_plot, width = 10, height = 6, dpi = 300, bg = "white")
  }
}, error = function(e) {
  cat("\nError in future forecasting:", conditionMessage(e), "\n")
  cat("Unable to generate future forecasts. Please review model specification.\n")
})

# ==============================================================================
# 6. SAVE RESULTS
# ==============================================================================
cat("\n==============================================================================\n")
cat("SAVING MODEL RESULTS\n")
cat("==============================================================================\n")

# Create a comprehensive results object
tryCatch({
  ets_results <- list(
    model_comparison = model_comparison,
    alpha_comparison = alpha_comparison,
    best_model_spec = best_spec,
    best_alpha = best_alpha,
    final_model = final_model,
    final_forecast = final_forecast,
    final_metrics = final_metrics,
    validation_actuals = validation_ts,
    validation_dates = validation_dates_df$date
  )
  
  # Add future forecast if available
  if (!is.null(future_forecast)) {
    ets_results$future_forecast <- future_forecast
  }
  
  # Save the results
  saveRDS(ets_results, "data/model_metrics/ets_model_results.rds")
  cat("\nResults saved to data/model_metrics/ets_model_results.rds\n")
}, error = function(e) {
  cat("\nError saving results:", conditionMessage(e), "\n")
  cat("Some results may not have been saved correctly.\n")
})

# Final message
cat("\n==============================================================================\n")
cat("ETS MODEL IMPLEMENTATION COMPLETED\n")
cat("==============================================================================\n")
cat("The ETS model has been fitted, evaluated, and used for forecasting.\n")
cat("Key results:\n")
cat("- Best model specification:", if(model_type == "auto") auto_ets_model$method else best_spec, "\n")
cat("- RMSE:", round(final_metrics$Value[1], 4), "\n")
cat("- Visualizations saved in visuals/4_ets_viz/ directory\n")
cat("- Complete results saved as RDS file\n")
cat("\nReady to proceed with ARIMA modeling.\n") 
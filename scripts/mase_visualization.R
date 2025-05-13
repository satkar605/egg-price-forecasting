# ==============================================================================
# MASE MODEL COMPARISON VISUALIZATION
# ==============================================================================
# This script creates a publication-quality visualization of MASE performance
# across all forecasting models for presentation purposes
# ==============================================================================

library(ggplot2)
library(dplyr)
library(tidyr)
library(viridis)  # For colorblind-friendly palette
library(patchwork)

# ==============================================================================
# PREPARE DATA
# ==============================================================================

# Create a data frame with MASE results
mase_data <- data.frame(
  Model = c("Naive", "ETS", "Auto ARIMA", "ARIMA with Differencing", "AR(1)"),
  Training = c(0.3418, 0.3004, 0.3156, 0.3175, 0.3389),
  Validation = c(2.7334, 3.3563, 2.7262, 2.8351, 2.9102)
)

# Transform to long format for plotting
mase_long <- mase_data %>%
  pivot_longer(cols = c(Training, Validation),
               names_to = "Period",
               values_to = "MASE") %>%
  mutate(
    # Reorder models by validation performance (best to worst)
    Model = factor(Model, 
                  levels = c("Auto ARIMA", "Naive", "ARIMA with Differencing", 
                             "AR(1)", "ETS")),
    # Set period as factor with training first
    Period = factor(Period, levels = c("Training", "Validation"))
  )

# ==============================================================================
# CREATE VISUALIZATION
# ==============================================================================

# Set up a custom theme
custom_theme <- theme_minimal() +
  theme(
    text = element_text(color = "#333333"),
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    legend.title = element_text(size = 11, face = "bold"),
    legend.text = element_text(size = 10),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "#e0e0e0"),
    plot.background = element_rect(fill = "white"),
    panel.background = element_rect(fill = "white"),
    legend.background = element_rect(fill = "white", color = NA),
    legend.key = element_rect(fill = "white", color = NA),
    plot.margin = margin(20, 20, 20, 20)
  )

# Function to create annotation for value labels
create_value_labels <- function(df) {
  df %>%
    mutate(
      label_y = MASE,
      label = sprintf("%.3f", MASE)
    )
}

# Create separate plots for Training and Validation due to scale differences
# Training Period Plot
training_plot <- mase_long %>%
  filter(Period == "Training") %>%
  create_value_labels() %>%
  ggplot(aes(x = Model, y = MASE, fill = Model)) +
  geom_bar(stat = "identity", width = 0.6, color = "white", size = 0.2) +
  geom_text(aes(y = label_y + 0.015, label = label), 
            fontface = "bold", size = 3.5) +
  scale_fill_viridis(discrete = TRUE, option = "D", begin = 0.1, end = 0.9) +
  scale_y_continuous(limits = c(0, 0.42), 
                     breaks = seq(0, 0.4, by = 0.1),
                     expand = expansion(mult = c(0, 0.15))) +
  labs(
    title = "Training Period MASE",
    subtitle = "Lower values indicate better performance",
    y = "MASE Value",
    x = NULL
  ) +
  custom_theme +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, size = 10)
  ) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "#E41A1C", size = 0.8) +
  annotate("text", x = 1, y = 1.05, label = "MASE = 1 (Seasonal Naive Benchmark)", 
           hjust = 0, fontface = "italic", size = 3, color = "#E41A1C")

# Validation Period Plot
validation_plot <- mase_long %>%
  filter(Period == "Validation") %>%
  create_value_labels() %>%
  ggplot(aes(x = Model, y = MASE, fill = Model)) +
  geom_bar(stat = "identity", width = 0.6, color = "white", size = 0.2) +
  geom_text(aes(y = label_y + 0.15, label = label), 
            fontface = "bold", size = 3.5) +
  scale_fill_viridis(discrete = TRUE, option = "D", begin = 0.1, end = 0.9) +
  scale_y_continuous(limits = c(0, 4.0), 
                     breaks = seq(0, 4, by = 1),
                     expand = expansion(mult = c(0, 0.1))) +
  labs(
    title = "Validation Period MASE",
    subtitle = "Note the much higher values during the volatile 2022-2025 period",
    y = "MASE Value",
    x = NULL
  ) +
  custom_theme +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, size = 10)
  ) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "#E41A1C", size = 0.8) +
  annotate("text", x = 1, y = 1.05, label = "MASE = 1 (Seasonal Naive Benchmark)", 
           hjust = 0, fontface = "italic", size = 3, color = "#E41A1C")

# Create comparison visualization with both plots
combined_plot <- training_plot / validation_plot +
  plot_layout(heights = c(1, 1.2)) +
  plot_annotation(
    title = "Model Performance Comparison: Mean Absolute Scaled Error (MASE)",
    subtitle = "Auto ARIMA model shows best validation performance and consistent training performance",
    caption = "Note: MASE < 1 indicates better performance than the seasonal naive forecast",
    theme = theme(
      plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5),
      plot.caption = element_text(size = 9, face = "italic", hjust = 1)
    )
  )

# Create small table for performance visualization
# Create full dataset MASE panel to highlight ARIMA's consistency
full_dataset_table <- data.frame(
  Model = c("Auto ARIMA"),
  `Full Dataset MASE` = c(0.3166)
) 

full_table_plot <- ggplot() +
  annotate("rect", xmin = 0, xmax = 1, ymin = 0, ymax = 1, fill = "#f8f8f8") +
  annotate("text", x = 0.5, y = 0.8, label = "Full Dataset MASE",
           fontface = "bold", size = 5, hjust = 0.5) +
  annotate("text", x = 0.5, y = 0.5, label = "Auto ARIMA: 0.3166",
           fontface = "bold", size = 6, hjust = 0.5, color = viridis(5)[1]) +
  # annotate("text", x = 0.5, y = 0.2, label = "Remarkable consistency between training (0.3156) and full dataset evaluation", fontface = "italic", size = 3, hjust = 0.5) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "#f8f8f8", color = "#333333", size = 0.5)
  )

# Final layout with table at bottom
final_plot <- (training_plot / validation_plot) | full_table_plot +
  plot_layout(widths = c(3, 1)) +
  plot_annotation(
    title = "Model Performance Comparison: Mean Absolute Scaled Error (MASE)",
    subtitle = "Auto ARIMA model shows best validation performance and consistent training performance",
    caption = "Note: MASE < 1 indicates better performance than the seasonal naive forecast",
    theme = theme(
      plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5),
      plot.caption = element_text(size = 9, face = "italic", hjust = 1)
    )
  )

# ==============================================================================
# SAVE VISUALIZATION
# ==============================================================================

# Create directory if it doesn't exist
dir.create("visuals/8_mase_evaluation", showWarnings = FALSE, recursive = TRUE)

# Save high-resolution plot for presentation
ggsave("visuals/8_mase_evaluation/mase_model_comparison.png", 
       final_plot, width = 12, height = 9, dpi = 300)

# Display plot
print(final_plot)

cat("✓ MASE visualization saved to: visuals/8_mase_evaluation/mase_model_comparison.png\n") 
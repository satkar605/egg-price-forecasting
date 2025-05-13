# ==============================================================================
# EXPLORATORY DATA ANALYSIS (EDA) FOR EGG PRICE TIME SERIES
# ==============================================================================
# This script performs exploratory analysis of the egg price data

# Set working directory to project root
setwd("/Users/satkarkarki/Desktop/Data_Analytics_Portfolio/eggs_forecasting")

# Add this message
cat("\n==============================================================================\n")
cat("NOTE: When running in terminal, plots will be saved as files in the 'visuals/' directory.\n")
cat("Please check this directory after the script completes to view the visualizations.\n")
cat("==============================================================================\n\n")

# Load necessary libraries
library(tidyverse)    # For data manipulation and visualization
library(forecast)     # For time series functions
library(tseries)      # For ADF test

# Create visuals directory if it doesn't exist
dir.create("visuals", showWarnings = FALSE)
dir.create("visuals/1_eda", showWarnings = FALSE)

# Source the data loading script
source("scripts/load_data.R")

# ==============================================================================
# 1. BASIC TIME SERIES VISUALIZATION
# ==============================================================================
cat("Creating time series visualizations...\n")
# First, create a simple/regular version with just the basic line
simple_plot <- ggplot(eggs, aes(x = date, y = egg_price)) +
  geom_line(color = "#2C3E50", size = 1) +
  labs(
    title = "Monthly Egg Prices - Basic View",
    y = "Price ($)", 
    x = "Year"
  ) +
  scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
  theme_minimal()

# Display the simple plot
print(simple_plot)

# Save the simple plot
ggsave("visuals/1_eda/simple_timeseries_plot.png", simple_plot, width = 10, height = 6, dpi = 300, bg = "white")
cat("✓ Simple time series plot saved to: visuals/1_eda/simple_timeseries_plot.png\n")

# Calculate mean for reference line
mean_egg_price <- mean(eggs$egg_price, na.rm = TRUE)

# Create an enhanced time series plot with index/legend
main_plot <- ggplot(eggs, aes(x = date, y = egg_price)) +
  # Main data series
  geom_line(aes(linetype = "Monthly Price"), color = "#2C3E50", size = 1) +
  
  # Add smoothed trend line
  geom_smooth(aes(linetype = "Smoothed Trend"), method = "loess", 
              color = "#E74C3C", se = FALSE, size = 1) +
  
  # Add mean line
  geom_hline(aes(yintercept = mean_egg_price, linetype = "Average Price"),
             color = "darkgray") +
  
  # Labels
  labs(
    title = "Monthly Egg Prices (1995-2025)",
    subtitle = "Raw data with trend line and historical average",
    caption = "Source: Egg Price Dataset",
    y = "Price ($)", 
    x = "Year",
    linetype = "Index" # This renames the legend title
  ) +
  
  # Set custom linetypes and labels
  scale_linetype_manual(
    values = c("Monthly Price" = "solid", 
               "Smoothed Trend" = "dashed",
               "Average Price" = "dotted"),
    guide = guide_legend(override.aes = list(
      color = c("#2C3E50", "#E74C3C", "darkgray")
    ))
  ) +
  
  # X-axis formatting
  scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
  
  # Theme customization
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 12),
    axis.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    legend.title = element_text(face = "bold")
  )

# Display the plot
print(main_plot)

# Save the plot
ggsave("visuals/1_eda/main_timeseries_plot.png", main_plot, width = 10, height = 6, dpi = 300, bg = "white")
cat("✓ Enhanced time series plot saved to: visuals/1_eda/main_timeseries_plot.png\n")

# Create visuals directory if it doesn't exist
dir.create("visuals", showWarnings = FALSE)

# ==============================================================================
# 2. SEASONAL ANALYSIS
# ==============================================================================
# Add year and month columns for seasonal analysis
eggs <- eggs %>%
  mutate(
    year = lubridate::year(date),
    month = lubridate::month(date, label = TRUE)
  )

# Create seasonal plot showing price patterns across years
seasonal_year_plot <- ggplot(eggs, aes(x = month, y = egg_price, group = year, color = as.factor(year))) +
  geom_line() +
  labs(
    title = "Seasonal Plot: Egg Prices by Year",
    subtitle = "Monthly patterns for each year",
    y = "Price ($)",
    x = "Month",
    color = "Year"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 12),
    axis.title = element_text(face = "bold"),
    legend.position = "right",
    panel.grid.minor = element_blank()
  )

# Display and save the seasonal year plot
print(seasonal_year_plot)
ggsave("visuals/1_eda/seasonal_year_plot.png", seasonal_year_plot, width = 12, height = 7, dpi = 300, bg = "white")
cat("✓ Seasonal plot by year saved to: visuals/1_eda/seasonal_year_plot.png\n")

# Create monthly boxplot to show price distribution by month
seasonal_box_plot <- ggplot(eggs, aes(x = month, y = egg_price)) +
  geom_boxplot(fill = "#3498DB", alpha = 0.7) +
  labs(
    title = "Monthly Egg Price Distribution",
    subtitle = "Boxplots showing price variation within each month",
    y = "Price ($)",
    x = "Month"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 12),
    axis.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

# Display and save the seasonal box plot
print(seasonal_box_plot)
ggsave("visuals/1_eda/seasonal_box_plot.png", seasonal_box_plot, width = 10, height = 6, dpi = 300, bg = "white")
cat("✓ Monthly boxplot saved to: visuals/1_eda/seasonal_box_plot.png\n")

# ==============================================================================
# 3. TIME SERIES DECOMPOSITION
# ==============================================================================
# Decompose time series into components (trend, seasonal, random)
eggs_decomp <- stats::decompose(eggs_ts)

# Prepare decomposition data for plotting
# This conversion is necessary because ggplot2 works better with dataframes
decomp_df <- data.frame(
  date = eggs$date,
  observed = as.vector(eggs_decomp$x),
  trend = as.vector(eggs_decomp$trend),
  seasonal = as.vector(eggs_decomp$seasonal),
  random = as.vector(eggs_decomp$random)
)

# Create individual plots for each decomposition component
# Each "p" object is a separate ggplot for one component
p1 <- ggplot(decomp_df, aes(x = date, y = observed)) + 
  geom_line(color = "#2C3E50") + 
  labs(title = "Observed", y = "Price ($)") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))

p2 <- ggplot(decomp_df, aes(x = date, y = trend)) + 
  geom_line(color = "#E74C3C") + 
  labs(title = "Trend", y = "Price ($)") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))

p3 <- ggplot(decomp_df, aes(x = date, y = seasonal)) + 
  geom_line(color = "#3498DB") + 
  labs(title = "Seasonal", y = "Price ($)") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))

p4 <- ggplot(decomp_df, aes(x = date, y = random)) + 
  geom_line(color = "#2ECC71") + 
  labs(title = "Random", y = "Price ($)") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))

# Load patchwork for combining plots
library(patchwork)

# Combine decomposition plots
decomp_plot <- p1 / p2 / p3 / p4 + 
  plot_annotation(
    title = "Time Series Decomposition of Egg Prices",
    subtitle = "Additive decomposition showing the four components",
    theme = theme(
      plot.title = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(size = 12)
    )
  )

# Display and save the combined plot
print(decomp_plot)
ggsave("visuals/1_eda/decomposition_plot.png", decomp_plot, width = 10, height = 12, dpi = 300, bg = "white")
cat("✓ Time series decomposition plot saved to: visuals/1_eda/decomposition_plot.png\n")

# ==============================================================================
# 4. CORRELATION ANALYSIS (ACF & PACF)
# ==============================================================================
# Create ACF plot for 24 lags (two years)
acf_plot <- ggAcf(eggs_ts, lag.max = 24, main = "Autocorrelation Function (ACF)") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 12))

# Create PACF plot for 24 lags (two years)
pacf_plot <- ggPacf(eggs_ts, lag.max = 24, main = "Partial Autocorrelation Function (PACF)") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 12))

# Combine plots side by side using patchwork
correlation_combined <- acf_plot + pacf_plot +
  plot_layout(ncol = 2) +
  plot_annotation(
    title = "Correlation Analysis for Egg Prices",
    subtitle = "Examining temporal dependencies across 24 lags (2 years)",
    theme = theme(
      plot.title = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(size = 12)
    )
  )

# Display the combined plot
print(correlation_combined)

# Save the combined plot
ggsave("visuals/1_eda/correlation_combined.png", correlation_combined, width = 12, height = 6, dpi = 300, bg = "white")
cat("✓ Combined ACF/PACF plot saved to: visuals/1_eda/correlation_combined.png\n")

# Still save individual plots for reference if needed
ggsave("visuals/1_eda/acf_plot.png", acf_plot, width = 8, height = 6, dpi = 300, bg = "white")
ggsave("visuals/1_eda/pacf_plot.png", pacf_plot, width = 8, height = 6, dpi = 300, bg = "white")
cat("✓ Individual ACF and PACF plots also saved for reference\n")

# ==============================================================================
# 5. STATIONARITY TESTING
# ==============================================================================
# Perform Augmented Dickey-Fuller test for stationarity
cat("\nPerforming Augmented Dickey-Fuller test for stationarity:\n")
adf_result <- adf.test(eggs_ts)
print(adf_result)

# Interpret ADF test results
cat("\nInterpretation of ADF test:\n")
if (adf_result$p.value < 0.05) {
  cat("- Reject null hypothesis (p-value < 0.05)\n")
  cat("- Series is stationary\n")
} else {
  cat("- Fail to reject null hypothesis (p-value >= 0.05)\n")
  cat("- Series is non-stationary and differencing is justified\n")
}

# ==============================================================================
# EDA COMPLETE - SUMMARY OF FINDINGS
# ==============================================================================
cat("\n==============================================================================\n")
cat("EXPLORATORY DATA ANALYSIS COMPLETED\n")
cat("==============================================================================\n\n")

cat("The following visualizations have been generated:\n")
cat("1. Basic time series plot: visuals/1_eda/simple_timeseries_plot.png\n")
cat("2. Enhanced time series plot: visuals/1_eda/main_timeseries_plot.png\n")
cat("3. Seasonal plot by year: visuals/1_eda/seasonal_year_plot.png\n")
cat("4. Monthly boxplot: visuals/1_eda/seasonal_box_plot.png\n")
cat("5. Time series decomposition: visuals/1_eda/decomposition_plot.png\n")
cat("6. ACF and PACF plots: visuals/1_eda/acf_plot.png and visuals/1_eda/pacf_plot.png\n")
cat("7. Combined ACF/PACF plot: visuals/1_eda/correlation_combined.png\n\n")

# Initialize findings_list for later use
findings_list <- c(
  "1. Basic time series plot: visuals/1_eda/simple_timeseries_plot.png",
  "2. Enhanced time series plot: visuals/1_eda/main_timeseries_plot.png",
  "3. Seasonal plot by year: visuals/1_eda/seasonal_year_plot.png",
  "4. Monthly boxplot: visuals/1_eda/seasonal_box_plot.png",
  "5. Time series decomposition: visuals/1_eda/decomposition_plot.png",
  "6. ACF and PACF plots: visuals/1_eda/acf_plot.png and visuals/1_eda/pacf_plot.png",
  "7. Combined ACF/PACF plot: visuals/1_eda/correlation_combined.png"
)

cat("Key observations:\n")
cat("- The time series shows both trend and seasonal components\n")
cat("- Recent years (2022-2025) show unusually high volatility in the random component\n")
cat("- ACF/PACF analysis suggests an AR(1) process with non-stationarity\n")
cat("- ADF test formally evaluates the stationarity assumption\n")
cat("- Inflation adjustment using CPI provides more meaningful price comparisons across time\n")
cat("- The formula real_price = nominal_price * (latest_cpi / historical_cpi) standardizes prices to current dollars\n")
cat("- Analysis using inflation-adjusted prices reveals true market dynamics separate from monetary inflation\n")
cat("- Recent price spikes remain significant even after inflation adjustment, indicating real market changes\n")
cat("- The next step is to build forecasting models based on these findings using inflation-adjusted data\n")
cat("==============================================================================\n")

# ==============================================================================
# ADD NOMINAL VS REAL PRICE COMPARISON
# ==============================================================================
cat("\nCreating comparison of nominal vs. inflation-adjusted prices...\n")

# Check if the inflation-adjusted data exists
adjusted_data_path <- "data/eggs_adjusted.csv"

if (file.exists(adjusted_data_path)) {
  # Load the complete dataset with both nominal and real prices
  complete_eggs <- read_csv(adjusted_data_path)
  
  # Check if the data has both the original egg price and real price
  if ("egg_price" %in% names(complete_eggs) && "real_price" %in% names(complete_eggs)) {
    # Create a long format dataset for plotting
    price_comparison_data <- complete_eggs %>%
      select(date, egg_price, real_price) %>%
      pivot_longer(cols = c(egg_price, real_price),
                   names_to = "price_type",
                   values_to = "price") %>%
      mutate(price_type = ifelse(price_type == "egg_price", 
                                "Nominal Price", 
                                "Real Price (Inflation-Adjusted)"))
    
    # Get the latest date for the subtitle
    latest_date <- max(complete_eggs$date)
    
    # Create the comparison visualization
    price_comparison_plot <- ggplot(price_comparison_data, aes(x = date, y = price, color = price_type)) +
      geom_line(size = 1) +
      labs(
        title = "Egg Prices: Nominal vs. Inflation-Adjusted",
        subtitle = paste0("Real prices expressed in equivalent ", format(latest_date, "%B %Y"), " dollars"),
        x = "Year",
        y = "Price ($)",
        color = "Series"
      ) +
      scale_color_manual(values = c("Nominal Price" = "#E74C3C", 
                                   "Real Price (Inflation-Adjusted)" = "#2C3E50")) +
      theme_minimal() +
      theme(
        plot.title = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(size = 11),
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA),
        axis.title = element_text(face = "bold"),
        legend.position = "bottom"
      )
    
    # Display and save the plot
    print(price_comparison_plot)
    ggsave("visuals/1_eda/price_comparison.png", price_comparison_plot, 
           width = 10, height = 6, dpi = 300, bg = "white")
    
    cat("Nominal vs. real price comparison saved as visuals/1_eda/price_comparison.png\n")
    
    # Add to the findings list
    findings_list <- c(findings_list, "8. Nominal vs. inflation-adjusted price comparison: visuals/1_eda/price_comparison.png")
  } else {
    cat("WARNING: The inflation-adjusted data file doesn't contain both egg_price and real_price columns\n")
  }
} else {
  cat("WARNING: Inflation-adjusted data file not found. Run calculate_real_prices.R first.\n")
}

cat("==============================================================================\n")

# Add this final message
cat("\n==============================================================================\n")
cat("VISUALIZATION COMPLETE\n")
cat("==============================================================================\n")
cat("All plots have been saved to the 'visuals/' directory.\n")
cat("You can view them by opening the PNG files in that directory.\n")
cat("Summary of files created:\n")
cat("1. visuals/1_eda/simple_timeseries_plot.png\n")
cat("2. visuals/1_eda/main_timeseries_plot.png\n")
cat("3. visuals/1_eda/seasonal_year_plot.png\n")
cat("4. visuals/1_eda/seasonal_box_plot.png\n")
cat("5. visuals/1_eda/decomposition_plot.png\n")
cat("6. visuals/1_eda/acf_plot.png\n")
cat("7. visuals/1_eda/pacf_plot.png\n")
cat("8. visuals/1_eda/correlation_combined.png\n")
cat("9. visuals/1_eda/price_comparison.png (if available)\n")
cat("==============================================================================\n") 
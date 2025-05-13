# ===============================================================================
# AUTO ARIMA FULL DATA FORECAST TABLE (12-MONTH HORIZON)
# ===============================================================================
library(tidyverse)
library(forecast)
library(lubridate)

# Load full inflation-adjusted data
data_path <- "data/eggs_adjusted.csv"
eggs <- readr::read_csv(data_path) %>%
  select(date, real_price) %>%
  rename(egg_price = real_price)
eggs$date <- as.Date(eggs$date)

# Create time series object for full data
start_year <- as.numeric(format(min(eggs$date), "%Y"))
start_month <- as.numeric(format(min(eggs$date), "%m"))
eggs_ts <- ts(eggs$egg_price, start = c(start_year, start_month), frequency = 12)

# Fit Auto ARIMA to full data
full_arima <- auto.arima(eggs_ts)
future_forecast <- forecast(full_arima, h = 12)
future_dates <- seq.Date(from = max(eggs$date) + months(1), by = "month", length.out = 12)

# Prepare forecast table
forecast_table <- tibble(
  Month = format(future_dates, "%b %Y"),
  Forecast = as.numeric(future_forecast$mean),
  Lower_95 = as.numeric(future_forecast$lower[,2]),
  Upper_95 = as.numeric(future_forecast$upper[,2])
)

# Save as CSV
outdir <- "visuals/12month_forecast"
dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
readr::write_csv(forecast_table, file.path(outdir, "autoarima_12month_forecast_feb2025_jan2026.csv"))
cat("\nAuto ARIMA 12-month forecast table (Feb 2025 - Jan 2026) saved to:", file.path(outdir, "autoarima_12month_forecast_feb2025_jan2026.csv"), "\n") 
# Forecasting Volatile Egg Prices for Procurement Planning

This project uses time-series forecasting to help Mister Smith’s, a local bakery in Vermillion, SD, manage the impact of volatile egg prices. After a sudden spike forced a $0.50 surcharge on egg-based dishes, the bakery needed a way to anticipate future price changes. By analyzing 30 years of monthly U.S. egg price data, the model delivers forward-looking insights to support better cost planning, supplier negotiations, and pricing decisions.

# Business Challenge
Mister Smith’s relies heavily on egg-based ingredients for products like quiches, pastries, and breakfast specials. When wholesale egg prices spiked unexpectedly in late 2024, the bakery was forced to apply a $0.50 surcharge across its egg-based dishes. This decision sparked customer complaints, affected foot traffic, and created inconsistencies in profit margins.
The underlying challenge is that without visibility into upcoming price movements, the bakery must react rather than plan. Volatile costs make it difficult to maintain stable pricing, manage inventory efficiently, or negotiate supplier contracts from a position of strength. The business needed a forecasting solution that could reduce uncertainty and support proactive cost planning.
This project aims to deliver just that: a monthly egg price forecast to guide procurement, pricing, and operational planning—transforming guesswork into informed decision-making.

# Key Business Questions
This project was driven by decision-oriented questions focused on operational stability, cost control, and customer experience:
## 1. What will egg prices look like over the next 12 months?  
Understanding price direction helps the bakery proactively manage pricing strategies and avoid abrupt menu changes.

## 2. When are egg prices most likely to spike seasonally?  
Identifying recurring high-cost months supports smarter bulk ordering and promotional planning.

## 3. What is the expected range of price volatility, and how should we plan for it?  
Forecast intervals give the bakery a risk band—helping decide when to build inventory buffers or secure fixed-price supplier agreements.

## 4. How reliable is the forecast, and can it be used for budgeting?  
The model’s performance on unseen data (2024) validates its usefulness for near-term financial planning

# Data Summary
The forecast model is built using publicly available economic data from the Federal Reserve Economic Data (FRED) platform, sourced directly from the U.S. Bureau of Labor Statistics (BLS). Two data series were used:
- **Nominal Egg Prices**: This series reports the average monthly price of Grade A, Large Eggs per dozen in the U.S. city average (unadjusted). It provides the historical pricing trend that directly reflects what businesses like Mister Smith’s would pay in the market.
- **Consumer Price Index (CPI) for Eggs**: This inflation index is specific to egg products and was used to adjust nominal prices for inflation. By doing so, the model reflects real price movements over time, enabling more meaningful trend analysis and forward-looking projections.

Together, these data sources provide a credible, inflation-adjusted time series of monthly egg prices from January 1995 through January 2025—spanning over 360 months of price activity. This long historical window improves the model’s ability to recognize seasonal patterns, detect structural breaks, and produce forecasts that are grounded in economic reality. The data required no imputation or cleaning, ensuring integrity and reliability for modeling purposes.  

![train_validation_split](https://github.com/user-attachments/assets/f81d2156-2667-4888-99a2-1705d33b2292)

# Analytical Approach
A careful evaluation of the egg price time series was performed to understand its underlying patterns and volatility. The decomposition chart below breaks the data into four components: observed prices, long-term trend, seasonality, and random fluctuations.
The trend component highlights periods of steady growth followed by a sharp escalation post-2020. Seasonal patterns are consistent across years, with regular spikes and dips aligned with typical demand cycles. However, the random component shows a clear structural shift in the last five years. Even after removing trend and seasonality, volatility remains high—suggesting short-term shocks are now a more dominant force. This posed a major challenge for accurate forecasting.  

![decomposition_plot](https://github.com/user-attachments/assets/8095eca4-60fc-491e-9289-8fdf94cc2c9e)

To address this, multiple time-series forecasting models were tested using a rolling validation approach. Each model was trained on historical data up to December 2023 and evaluated on its ability to predict prices for 2024. Three models were compared:
- **Naïve Model**: Used as a benchmark.
- **Exponential Smoothing (ETS)**: Captured regular seasonal patterns.
- **ARIMA**: Selected for its strength in economic forecasting and ability to model autocorrelations and structural patterns effectively.

ARIMA was chosen as the final model based on its superior predictive accuracy across all validation metrics. Technical steps such as stationarity testing, differencing, and model selection were applied with rigor to ensure reliability. Despite the volatility in recent years, ARIMA was best suited to balance forecast accuracy with model interpretability.

# Findings and Interpretations
1. **Winter price spikes are consistent.** A seasonal trend is clearly visible in the historical data, where prices rise during the colder months and drop in summer. This pattern appears reliably from November through January, making it a key planning window. As shown in the monthly seasonal boxplot below, median prices in December and January are among the highest, with greater variability. This indicates both a recurring spike and higher risk. This insight supports early ordering in Q4 to secure better margins and reduce the need for last minute supplier decisions.

![seasonal_box_plot](https://github.com/user-attachments/assets/deeefcc1-0cb2-46be-837b-fb979d49844d)

2. **Recent volatility breaks the pattern.** Price swings in the last few years are wider and harder to predict, likely driven by external shocks.

3. **The ARIMA model performed best.** It captured overall trends well but underpredicted sharp increases in late 2024.

4. **Forecast error increased near the end.** Residuals show the model struggled most during recent instability.

**Key takeaway**: While the model supports short-term planning, a buffer strategy is needed to manage ongoing uncertainty. The fully-generated forecasted is as follows;  

![full_egg_price_timeline](https://github.com/user-attachments/assets/4a738af4-1fa0-4bdb-8278-43ec8f0beeb5)

# Business Recommendations
1. **Order early for November–January to avoid seasonal spikes**: Prices historically peak during these winter months. Advance purchasing for egg-heavy inventory (e.g., quiches, breakfast items) can reduce exposure to price hikes of $0.80–$1.20 per dozen.

2. **Use $3.40 as a planning anchor for egg costs**: The model forecasts prices to stabilize near $3.40/dozen through early 2026. This can serve as a reference point when budgeting or reviewing supplier offers.

3. **React to price triggers using forecast range**: The 95% forecast interval suggests prices could swing between $2.30 and $4.70/dozen.  
   - If prices fall below $3.00, consider locking in bulk purchase contracts.  
   - If prices rise above $4.50, prepare to increase buffer inventory or evaluate menu pricing adjustments.

4. **Minimize sudden customer-facing changes**  
Instead of reactive $0.50 surcharges, use forecast trends to plan incremental price changes that align with expected cost movements. This protects both margin and customer loyalty.

# Assumptions and Limitations
1. **No external predictors were used**: The model is based only on historical egg prices. It does not include external factors such as feed costs, transportation, or disease outbreaks, which can significantly affect price movements.

2. **Stationarity testing suggested differencing, but the final model did not apply it**: The Augmented Dickey-Fuller test indicated non-stationarity. However, the best-performing model selected by auto.arima() was ARIMA(4,0,0)(2,0,0)[12], which used no differencing. AR(1) models with and without differencing were also tested but did not outperform this model.

3. **Forecast assumes past patterns will continue**: The model assumes that seasonal and structural patterns seen in the past will hold in the future. Unexpected market shocks or policy changes could reduce its predictive accuracy.

_For a detailed walkthrough of the modeling process, validation strategy, and forecasting logic, refer to the technical scripts and visuals provided in the `scripts/` and `visuals/` directories._


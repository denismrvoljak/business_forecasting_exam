# What is the main variable of interest? Does this variable exhibit seasonality or trends
# over the observed time period? Use two diﬀerent approaches to identify and quantify
# these eﬀects, or demonstrate that they are not present.

#Weather temperature relevant as:
# The reasoning is
# simple: colder days may drive higher demand for hot drinks, while warmer days may reduce
# it.


# ============= Data Load =============

library(readxl)

# Load data
data <- read_excel("exam/Exam_Jan2025.xlsx")

# Load individual columns


sales = data$tr #  Total Revenue from sales per day

avg_temp <- data$tavg # Average temperature per day


# Load individual columns as  time-series objects

sales_ts <- ts(data$tr, start=1, frequency=7)

avg_temp_ts = ts(data$tavg, start=1, frequency=7)


# ============= Dynamic Properties =============
library(forecast)
library(stats)

# Visual inspection
plot(sales_ts)

acf(sales_ts)

tsdisplay(sales_ts)
# From the ACF, we can see it doesn't die out -> suggests non-stationarity
# We can also notice seasonality in the data

# Statistical test for stationarity
library(tseries)
adf.test(sales_ts)
# H0: series has unit root (non-stationary)
# If p < 0.05, reject H0 -> series is stationary

# The adf test shows that the series is stationary with p-value of 0.0366


sales_decomposition <- decompose(sales_ts, type = "additive")
plot(sales_decomposition)

# Extract components
trend_comp <- sales_decomposition$trend
seasonal_comp <- sales_decomposition$seasonal
random_comp <- sales_decomposition$random

# Regression with trend and seasonal components
month_dummies_seasonal <- seasonaldummy(sales_ts) #create dummies for months
time_trend_seasonal <- seq(1:length(sales_ts)) #create a time trend variable

seasonal_model <- lm(sales_ts ~ month_dummies_seasonal + time_trend_seasonal)

summary(seasonal_model)

# Q2. Considering the entrepreneur’s interest in the impact of weather, which variable (e.g.,
# average temperature, precipitation, or another weather factor) will you select as the
# primary focus for the analysis? Examine its properties.

plot(avg_temp_ts)

acf(avg_temp_ts)

tsdisplay(avg_temp_ts)

adf.test(avg_temp_ts)
# The adf test shows that the series is not-stationary with p-value of 0.62

avg_temp_diff <- diff(avg_temp_ts)

tsdisplay(avg_temp_diff)

adf.test(avg_temp_diff)
# now avg temp is stationary



# Given your choices in (1) and (2), you now want to run a regression to estimate the
# relationship between these variables. What properties of the variables do you need to
# consider before estimating the regression of interest? Given the setup and the informa-
# tion available, will you run a static or a predictive regression? Elaborate.


# ============= Unit Root and Cointegration Analysis =============

# Sales do not have unit root

# temperature hs unit root -> so we will need to difference it, so it's stationary


## ================== Cointegration Analysis  ==================


# Combine variables for cointegration testing
combined_series <- ts(cbind(sales_ts, avg_temp_ts))

# Phillips-Ouliaris cointegration test (Automatic Engle-Granger cointegration test)
# H0: no cointegration
# H1: series are cointegrated
po.test(combined_series)
# If the test statistic is lower than the critical value at the 5% level, the series are cointegrated ->  long-term equilibrium relationship between variables

# The test statistic is lower than critical values, so the series is co-integrated and we can use both in the regression

# Required libraries
library(forecast)
library(lmtest)    # For regression diagnostics
library(car)       # For VIF test

## ============= Simple Static Regression =============

# Data should not be time-series objects

lm_model <- lm(sales_ts ~ avg_temp_ts)

# Visual analysis
plot(sales_ts, avg_temp_ts)
abline(lm_model, col = "red", lwd = 2)  # Add regression line (doesn't work really)

# Creating a scatter plot with a smooth-line fitting
scatter.smooth(x=sales_ts, y= avg_temp_ts, main="Sales ~ Average Temperature")

# not really a good linear fit

# Check correlation
cor(sales_ts, avg_temp_ts)


#[1] -0.1298527 -> sales go down when temperature is increasing

# Model summary
summary(lm_model)

## ===== Diagnostics for Regression =========

# 1. Check residuals visually
plot(resid(lm_model), main = "Residual Plot", 
     xlab = "Fitted Values", ylab = "Residuals", 
     pch = 16, col = "blue")
abline(h = 0, col = "red", lty = 2)
# Residual plot should show no clear pattern (random scatter).
# Patterns indicate potential issues like non-linearity or heteroscedasticity.

acf(resid(lm_model), main = "ACF of Residuals")
# ACF plot visually checks for autocorrelation in residuals.
# Significant spikes outside confidence intervals suggest autocorrelation.

# 2. Test for heteroscedasticity (Breusch-Pagan Test)
library(lmtest)
bptest(lm_model)
# H0: Residuals are homoscedastic (constant variance).
# If p < 0.05, reject H0 -> Residuals are heteroscedastic (non-constant variance).
# # Residuals seem to exhibit constant variance

# 3. Test for autocorrelation (Durbin-Watson Test)
dwtest(lm_model)
# H0: No autocorrelation in residuals.
# If p < 0.05, reject H0 -> Residuals are autocorrelated.
# Consider using time series methods or adding lagged variables if autocorrelation exists.
## Autocorrelated residuals

# 4. Test for stationarity (Augmented Dickey-Fuller Test)
library(tseries)
adf.test(resid(lm_model))
# H0: Residuals have a unit root (non-stationary).
# If p < 0.05, reject H0 -> Residuals are stationary.
# Stationarity is essential for valid regression in time series data.
# # here p-value is close to 4.9%, so residuals are stationary


# =Interpretation
# - Use the visual and formal diagnostic results to identify model issues.
# - Address detected problems:
#   - Heteroscedasticity: Transform variables, or use `vcovHC` for robust standard errors.
#   - Autocorrelation: Use ARIMA or GLS models instead of simple regression.
#   - Non-stationarity: Differencing or detrending may help achieve stationarity.




# For the remaining analysis, split your data into two parts where the pseudo-out-of-
#   sample contains 49 observations. Use the in-sample data to estimate the regression
# model specified in (3). Examine the residuals of this regression as well as its out-of-
#   sample performance. What can you conclude regarding the relationship between the
# variables?

# ================== Data Spliit ==================

split_point = nrow(data)-49

# Convert to time-series object

# Get time points
time_points <- time(sales_ts)

# Training set
insample_sales <- ts(sales_ts[1:split_point],
               start=time_points[1],
               end=time_points[split_point],
               frequency=7) # change to your frequency
# Test set 
outsample_sales <- ts(sales_ts[(split_point+1):length(sales_ts)],
                start=time_points[split_point+1],
                end=time_points[length(sales_ts)],
                frequency=7) # change to your frequency

# Training set
insample_temp <- ts(avg_temp_ts[1:split_point],
                     start=time_points[1],
                     end=time_points[split_point],
                     frequency=7) # change to your frequency
# Test set 
outsample_temp <- ts(avg_temp_ts[(split_point+1):length(avg_temp_ts)],
                      start=time_points[split_point+1],
                      end=time_points[length(avg_temp_ts)],
                      frequency=7) # change to your frequency


# To verify alignment:
print(time(insample_temp)[length(insample_temp)])  # Last time point of training
print(time(outsample_temp)[1])              # First time point of test



# Combine variables for cointegration testing
combined_series <- ts(cbind(insample_sales, insample_temp))

# Phillips-Ouliaris cointegration test (Automatic Engle-Granger cointegration test)
# H0: no cointegration
# H1: series are cointegrated
po.test(combined_series)
# If the test statistic is lower than the critical value at the 5% level, the series are cointegrated ->  long-term equilibrium relationship between variables
# Still co-integrated


# ============= Simple Static Regression 2 =============

# Data should not be time-series objects

lm_model_2 <- lm(insample_sales ~ insample_temp)

# Creating a scatter plot with a smooth-line fitting
scatter.smooth(x=insample_sales, y= insample_temp, main="Sales ~ Average Temperature")

# not really a good linear fit

# Check correlation
cor(insample_sales, insample_temp)

#[1] -0.1298527 -> sales go down when temperature is increasing

# Model summary
summary(lm_model_2)

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)    350.228     23.161  15.122   <2e-16 ***
# insample_temp   -4.058      1.323  -3.067   0.0024 ** 

## ===== Diagnostics for Regression =========

# 1. Check residuals visually
plot(resid(lm_model_2), main = "Residual Plot", 
     xlab = "Fitted Values", ylab = "Residuals", 
     pch = 16, col = "blue")
abline(h = 0, col = "red", lty = 2)
# Residual plot should show no clear pattern (random scatter).
# Patterns indicate potential issues like non-linearity or heteroscedasticity.

acf(resid(lm_model_2, main = "ACF of Residuals"))
# ACF plot visually checks for autocorrelation in residuals.
# Significant spikes outside confidence intervals suggest autocorrelation.

# 2. Test for heteroscedasticity (Breusch-Pagan Test)
library(lmtest)
bptest(lm_model_2)
# H0: Residuals are homoscedastic (constant variance).
# If p < 0.05, reject H0 -> Residuals are heteroscedastic (non-constant variance).
# # Residuals seem to exhibit constant variance

# 3. Test for autocorrelation (Durbin-Watson Test)
dwtest(lm_model_2)
# H0: No autocorrelation in residuals.
# If p < 0.05, reject H0 -> Residuals are autocorrelated.
# Consider using time series methods or adding lagged variables if autocorrelation exists.
## Autocorrelated residuals

# 4. Test for stationarity (Augmented Dickey-Fuller Test)
library(tseries)
adf.test(resid(lm_model_2))
# H0: Residuals have a unit root (non-stationary).
# If p < 0.05, reject H0 -> Residuals are stationary.
# Stationarity is essential for valid regression in time series data.
# # here p-value is close to 4.9%, so residuals are stationary


# OUT oo sample performance
forecast_lm_model_2 <- forecast(lm_model_2, newdata = data.frame(insample_temp = outsample_temp), h = 49)
predicted_values <- forecast_lm_model_2$mean

# Evaluate forecast accuracy
reg_acc = accuracy(predicted_values, outsample_sales)
reg_acc

#test set RMSE
simple_regression_rmse = reg_acc[2]
simple_regression_rmse

# Maybe a plot could be nice

# Using the estimated regression, provide a forecast for the rest of December for the
# variable of interest. If relevant, use the forecast information available for the weather
# variables.

# I will use the regression estimated in the exerices (3), because it is estimated on the whole dataset. I will use the forecasted temperature for the rest of December to forecast the sales.


# ============= Simple Static Regression Forecast =============


# Load data
forecast_data <- read_excel("exam/forecasted_data.xlsx")

avg_temp_forecasted <- forecast_data$tavg # Average temperature per day forecasted

# Forecast for new value


forecast_results_end_of_year <- forecast(lm_model, newdata = data.frame(avg_temp_ts = avg_temp_forecasted), h = 8)

end_of_year_forecast = forecast_results_end_of_year$mean

#PLOT
sales_adjusted = sales_ts[200:294]
# Combine original data with forecasts (excluding overlapping observation)
y_combined <- c(sales_adjusted, end_of_year_forecast) # Excluding overlapping observation  # change for number of periods ahead

time_index <- 1:length(y_combined)

# Visualize results
plot(time_index, y_combined, type = "l", 
     xlab = "Time", ylab = "Y", 
     main = "Original and Forecasted Y", 
     col = "blue")
lines(time_index[1:length(sales_adjusted)], sales_adjusted, col = "black", lty = 1)
abline(v = length(sales_adjusted), col = "red")
legend("topleft", 
       legend = c("Original Y", "Forecasted Y"), 
       col = c("black", "blue"), 
       lty = 1)


# Estimate an ARIMA model for the main variable of interest. Does the selected model
# align with the patterns identified earlier? Evaluate the performance of the model.

# ================== ARIMA Modeling  ==================
# Required Libraries
library(forecast)
library(stats)
library(tseries)

# Fitting an ARIMA model to the in-sample window
auto_arima <- auto.arima(insample_sales, seasonal = TRUE)
summary(auto_arima)
# ARIMA(1,1,1) -> order of integration does not align -> Check acf and pacf

acf(sales_ts)
pacf(sales_ts)

tsdisplay(residuals(auto_arima), main='Model Residuals') # Checking the model diagnostics
# comment on residuals: no clear pattern, no autocorrelation

Box.test(residuals(auto_arima), lag = 7, type = "Ljung-Box")
# H0: residuals are independently distributed (are white noise)
# If p < 0.05, reject H0 -> residuals are not independently distributed
#White noise

##==== Forecast (Generate forecasts using arima model) =====
auto_arima_forecast <- forecast(auto_arima, h=length(outsample_sales))

# Plot forecasts with confidence intervals
plot(auto_arima_forecast)

# Add actual data to plot to compare
forecasted_values <- ts(auto_arima_forecast$mean, start = time_points[split_point+1], frequency=7)  # Get forecast means

plot(outsample)  # Add actual values
lines(forecasted_values, col='blue')  # Plot forecasts # Swap with lines if you get ugly graph

# Check out of sample forecast accuracy
accuracy(auto_arima_forecast$mean, outsample_sales) #accuracy for arima is reported on the original scale

# Out of sample RMSE
auto_arima_out_sample_rmse = accuracy(auto_arima_forecast$mean, outsample_sales)[2]
auto_arima_out_sample_rmse

# Create comparison table of forecasts vs actuals
print(data.frame(forecast = auto_arima_forecast$mean, actual = outsample_sales))

# ================== Compare Models  ==================

results = data.frame(
  regression_outsample_rmse = simple_regression_rmse,
  arima_outsample_rmse = auto_arima_out_sample_rmse
)

print(results)

# regression_outsample_rmse arima_outsample_rmse
# 1                  132.8966             156.4094


#Statistical comparison

# Compare forecasting methods using Diebold-Mariano test
# H0: Both methods have equal forecast accuracy
dm_test_residuals <- dm.test(residuals(auto_arima_forecast), residuals(forecast_lm_model_2), h=length(outsample_sales)) #the null hypothesis is that the two methods have the same forecast accuracy. 

dm_test_errors <- dm.test(auto_arima_forecast$mean - outsample_sales, forecast_lm_model_2$mean - outsample_sales, h=length(outsample_sales)) #WE can compare the performance of the models out-of-sample

dm_test_residuals
dm_test_errors

# Diebold-Mariano Test
# 
# data:  residuals(auto_arima_forecast)residuals(forecast_lm_model_2)
# DM = -1.7083, Forecast horizon = 49, Loss function power = 2, p-value = 0.08886
# alternative hypothesis: two.sided
# 
# > dm_test_errors
# 
# Diebold-Mariano Test
# 
# data:  auto_arima_forecast$mean - outsample_salesforecast_lm_model_2$mean - outsample_sales
# DM = 0, Forecast horizon = 49, Loss function power = 2, p-value = 1
# alternative hypothesis: two.sided

# We are not able to reject the null, so the forecasts are of equal predictive ability


# ============= Forecast Combinations =============

# Load required libraries
library(readxl)
library(forecast)
library(ggplot2)
library(tseries)

## ================== 3. Equal weights method  ==================
equal_weights_forecast <- 0.5 * auto_arima_forecast$mean + 0.5 * forecast_lm_model_2$mean
equal_weights_accuracy <- accuracy(equal_weights_forecast, outsample_sales)
equal_weights_accuracy

equal_weights_rmse = equal_weights_accuracy[2]

## ================== 2. Granger-Ramanathan combination method  ==================

gr_fit <- lm(outsample_sales ~ auto_arima_forecast$mean + forecast_lm_model_2$mean)

gr_forecast <- ts(gr_fit$fitted.values, start=time_points[split_point+1],
                  end=time_points[length(sales_ts)],frequency=7)

gr_accuracy <- accuracy(gr_forecast, outsample_sales)
gr_accuracy


# Plot comparison of methods (Black: Actual, Blue: GR, Red: Equal Weights)
ts.plot(outsample_sales, gr_forecast, equal_weights_forecast, 
        gpars=list(xlab="Month", lty=c(1:3)), 
        col=c("black", "blue", "red"))

# RMSE
gr_rmse = gr_accuracy[2]

rmse_results_df = data.frame(
  regression_outsample_rmse = simple_regression_rmse,
  arima_outsample_rmse = auto_arima_out_sample_rmse,
  equal_weights_rmse = equal_weights_rmse,
  gr_rmse = gr_rmse
)

rmse_results_df


# Final forecast

# As the data ends on 24th of December, we need to produce forecast for remaining days of December -> 8 days

# FUll simple reg model:

forecast_results_end_of_year

# arima on all data

# Alternative: Manual specification if we want to enforce seasonal ARIMA
manual_arima <- Arima(sales_ts, 
                      order=c(1,1,1))                         # (p,d,q

summary(manual_arima)


manual_arima_forecast <- forecast(auto_arima, h=8)

manual_arima_forecast_mean = as.numeric(manual_arima_forecast$mean)

lm_model_forecast_mean = forecast_results_end_of_year$mean

manual_arima_forecast_mean

lm_model_forecast_mean


summary(gr_fit)

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)
# (Intercept)              -1.416e+05  3.206e+05  -0.442    0.661
# auto_arima_forecast$mean  3.678e+02  8.311e+02   0.443    0.660
# forecast_lm_model_2$mean -9.613e-02  1.608e+00  -0.060    0.953



Combined_forecast = (-1.416e+05) + (3.678e+02 * manual_arima_forecast_mean) + (-9.613e-02 * lm_model_forecast_mean)

Combined_forecast


# 1        2        3        4        5        6        7        8 
# 231.5922 285.7572 285.0295 285.3416 285.3241 285.4765 285.6604 285.8076 
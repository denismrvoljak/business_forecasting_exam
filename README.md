# # Aarhus Univesity | Business Forecasting Exam | January 2025

This repository contains my exam solutions demonstrating advanced time series analysis and forecasting techniques using R. The case study focused on analyzing sales data and weather impacts for a coffee vending machine business.

## Methods Covered

### Time Series Analysis
- Time series decomposition
- Trend and seasonality analysis
- Stationarity testing using Augmented Dickey-Fuller test
- ACF and PACF analysis
- Cointegration testing using Phillips-Ouliaris test

### Regression Analysis
- Static regression modeling
- Residual diagnostics
- Out-of-sample performance evaluation
- Heteroscedasticity testing (Breusch-Pagan Test)
- Autocorrelation testing (Durbin-Watson Test)

### Forecasting Techniques
- ARIMA modeling
  - Model selection using auto.arima
  - Manual ARIMA specification
  - Residual analysis
- Forecast combinations
  - Equal weights method
  - Granger-Ramanathan combination
- Out-of-sample forecast evaluation
- Diebold-Mariano test for forecast comparison

### Data Analysis
- Dynamic property analysis
- Training/test set splitting
- Performance metrics calculation (RMSE, MSE)
- Visualization of results using ggplot2

## Technical Skills Demonstrated

### Statistical Analysis
- Unit root testing
- Cointegration analysis
- Time series decomposition
- Forecast accuracy evaluation

### Programming
- R statistical programming
- Time series data manipulation
- Advanced data visualization
- Model implementation

### Libraries Used
```R
library(forecast)
library(stats)
library(tseries)
library(lmtest)
library(ggplot2)
library(readxl)
library(car)
```

## Key Components

### 1. Time Series Decomposition
- Identified trends and seasonal patterns in sales data
- Analyzed weekly and long-term seasonality
- Implemented both additive and regression-based decomposition

### 2. Temperature Impact Analysis
- Analyzed relationship between temperature and sales
- Implemented cointegration testing
- Developed static regression models

### 3. Forecasting Models
- ARIMA modeling and diagnostics
- Combined forecasting approaches
- Out-of-sample performance evaluation

### 4. Business Recommendations
- Evidence-based insights for business decision-making
- Strategic recommendations for location optimization
- Sales forecast for future periods

## Results Summary
- Demonstrated significant relationship between temperature and sales
- Identified optimal forecasting approach using combination methods
- Provided actionable business insights based on analysis
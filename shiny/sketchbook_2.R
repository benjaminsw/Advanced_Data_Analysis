# ref for code: https://www.listendata.com/2015/10/arima-modeling-with-r.html 
# ref for seasonality: https://online.stat.psu.edu/stat510/lesson/4/4.2 
################################################################################
# Plot the time series data
# Check volatility - Run Box-Cox transformation to stabilize the variance
# Check whether data contains seasonality. If yes, two options - either take seasonal differencing or fit seasonal arima model.
# If the data are non-stationary: take first differences of the data until the data are stationary 
# Identify orders of p,d and q by examining the ACF/PACF
# Try your chosen models, and use the AICC/BIC to search for a better model. 
# Check the residuals from your chosen model by plotting the ACF of the residuals, and doing a portmanteau test of the residuals. If they do not look like white noise, try a modified model.
# Check whether residuals are normally distributed with mean zero and constant variance 
# Once step 7 and 8 are completed, calculate forecasts
################################################################################
# Load Data
library(forecast)
library(fpp)
# Plot time series data
tsdisplay(condmilk)

# Step I: Check Volatility
# Lambda values : 
# λ = 1  (No substantive transformation)
# λ = 0.5 (Square root plus linear transformation)
# λ = 0 (Natural logarithm)
# λ = −1 (Inverse plus 1)
lambda = BoxCox.lambda(condmilk)
tsdata2 = BoxCox(condmilk, lambda=lambda)
tsdisplay(tsdata2)

# Step 2: detect Seasonality
seasonplot(condmilk)
monthplot(condmilk)

# Step 3: Detect Non-Stationary Data
# Unit Ratio Tests
library(tseries)
adf = adf.test(tsdata2)
kpss = kpss.test(tsdata2)
adf
kpss

# treat Non-Stationary Data
# Number of Difference Required to make data stationary
n = ndiffs(tsdata2)
tsdata3 = diff(tsdata2, differences = n)
plot.ts(tsdata3)

# Step 4: Model Identification and Estimation
# Method I: ACF and PACF Functions
acf(tsdata2, lag.max = 20)
pacf(tsdata2, lag.max = 20)

# Method II: Minimum AIC / BIC Criteria
#Automatic Selection Algorithm - Fast
auto.arima(tsdata2, trace= TRUE, ic ="aicc", approximation = FALSE)
#Auto Algorithm - Slow but more accurate
auto.arima(tsdata2, trace= FALSE, ic ="aicc", approximation = FALSE, stepwise = FALSE)
#Compare Multiple Models
AIC(arima(tsdata2, order = c(1, 0, 0), seasonal = list(order = c(2,0,0), period = 12)),
    arima(tsdata2, order = c(2, 0, 0), seasonal = list(order = c(2,0,0), period = 12)),
    arima(tsdata2, order = c(0, 0, 3), seasonal = list(order = c(2,0,0), period = 12)))
finalmodel = arima(tsdata2, order = c(0, 0, 3), seasonal = list(order = c(2,0,0), period = 12))
summary(finalmodel)
#Residual Diagnostics
#1. Residuals are Uncorrelated (White Noise)
#2. Residuals are normally distributed with mean zero
#3. Residuals have constant Variance
# Check whether the residuals look like white noise (Independent)
# p>0.05 then the residuals are independent (white noise)
tsdisplay(residuals(finalmodel))
Box.test(finalmodel$residuals, lag = 20, type = "Ljung-Box")
# p-values shown for the Ljung-Box statistic plot are incorrect so calculate
#critical chi squared value
# Chi-squared 20 d.f. and critical value at the 0.05
qchisq(0.05, 20, lower.tail = F)
# Observed Chi-squared 13.584 < 31.41 so we don't reject null hypothesis
# It means residuals are independent or uncorrelated (white noise) at lags 1-20.  
# whether the forecast errors are normally distributed
qqnorm(finalmodel$residuals); qqline(finalmodel$residuals) # Normality Plot

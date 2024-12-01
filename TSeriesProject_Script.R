---
title: "Time Series Project"
author: "Student Name"
date: "`r Sys.Date()`"
output:
  
pdf_document: default
  
---


## This file contain the solution to my final time series project

## import necessary libraries

library(forecast)
library(xts)
library(ggplot2)
library(dplyr)
library(TTR)
library(MASS)
library(tseries)


## set the dataset dir

## the location can be changed to match the dataset location of the current machine


current_directory <- getwd()
print(current_directory)


## reading the dataset

df <- read.csv("RDatasets/NIFTY50_all.csv",header = TRUE)
column_names <- names(df)
print(column_names)

#printing 5 rows of the df
five_rows <- head(df, 5)
print(five_rows)



## Data preprocessing



## getting number of rowa
row_count <- nrow(df)
print(row_count)

#checking for missing values
missing_values <- sum(is.na(df))
print(missing_values)



##convert data types to numeric
df$VWAP <- as.numeric(df$VWAP)
df$Volume <- as.numeric(df$Volume)

# Convert 'datesold' column to proper date format
df$Date <- as.Date(df$Date)

class(df$Date)
class(df$VWAP)
class(df$Volume)

#display summery
print("---- Summary of the dataset--------")
summary(df)



## Lets convert the dataframe above into a time series object

## the data set has irregular intervals, therefore i have used a frequency of 1


#Converting data into a ts object with irregular frequency
ts_df <- ts(df$VWAP,start=2000,end=2021, frequency = 1)
head(ts_df)



## lets Explore and visualize the time series

## this will help us understand its characteristics.


plot(ts_df, xlab = "Date", ylab = "VWAP")



## from above, plot is noise, since the interval of the data set is irregular,

## we can transform it to a monthly dataset


# Create an xts object with the data
df_xts <- xts(df[, c("Date", "VWAP","Volume")], order.by = df$Date)

# Aggregate to monthly intervals using the first observation in each month
df_monthly <- apply.monthly(df_xts, function(x) x[1, ])

# Display the first few rows of the modified time series
print(df_monthly)



## lets view the times series based on vwap


ts_df <- ts(df_monthly$VWAP,start=2000,end=2021, frequency = 12)

plot.ts(ts_df, main = "VWAP Time Series", ylab = "VWAP")





## lets view the times series based on VOlume



ts_df_vol <- ts(df_monthly$Volume,start=2000,end=2021, frequency = 12)

plot(ts_df_vol, main = "volume Time Series", ylab = "volume")



## checking if our model is adequate

## Augmented Dickey-Fuller (ADF) Test:

## The Augmented Dickey-Fuller (ADF) test is a statistical test used to determine whether a time series has a unit root or is stationary


ts_data <- ts(df_monthly$Volume,start=2000,end=2021, frequency = 12)

ts_data <- as.numeric(ts_data)
ts_data <- tsclean(ts_data)
# Perform Augmented Dickey-Fuller test
result <- adf.test(ts_data)

# Print the test results
print(result)



## from the above test results

## "Dickey-Fuller = -2.7316" is the test statistic value calculated by the ADF test. -\> "Lag order = 5" indicates that 5 lags were considered in the test. -\> "p-value = 0.03442" is the p-value

## Reject the hypothesis

## From the test results above, the p-value is 0.099, which is greater than the typical significance level of 0.05. Hence, we fail to reject the null hypothesis of having a unit root, therefore it means that, my time series non-stationary.

## using the decomposition test

## Autocorrelation and Partial Autocorrelation Plots:


# Plot ACF and PACF
acf(ts_data)
pacf(ts_data)


## Summary

##From the above decomposition test, it further means that our time series object is not stationary, 
##it cant fit to the arima model

##Detrending to strationarize the time series object



##Steps to make the ts stationary

model <- lm(ts_data ~ time(ts_data))
fitted_values <- predict(model)
stationary_ts <- ts_data - fitted_values


## lets perform another test to check if its stationary_ts
# Perform Augmented Dickey-Fuller test
result <- adf.test(stationary_ts)

# Print the test results
print(result)

#results are same, we use another method

## lets stabilize the variance by using log
log.ts_monthly <- log(ts_data)

## get the square root of the ts_data time series
sqrt.ts_monthly <- sqrt(ts_data)

##lets fit lenear regression model
t = 1:length(ts_data) 
fit = lm(ts_data ~ t) 
## applying Box-Cox Transformation
bc_transform = boxcox(ts_data ~ t,plotit = TRUE)
##performing Power Transformation, Based on the Box-Cox transformation
lambda = bc_transform$x[which(bc_transform$y == max(bc_transform$y))] 

ts_dat_bc = (1/lambda)*(ts_data^lambda-1)




## ploting the transformations


op= par(mfrow=c(2,2)) 
plot.ts(ts_data, main = "This is original t-series with vwap") 
plot.ts(log.ts_monthly, main = "Log Transform ") 
plot.ts(sqrt.ts_monthly, main = "Square Root Transform")
plot.ts(ts_dat_bc, main = "T series after Box-Cox Transform ")


var(ts_data)
var(ts_dat_bc)


## Decomposing the results to remove the trends



ts_clean <- tsclean(ts_dat_bc)

# Perform first-order differencing
differenced_ts <- diff(ts_clean,lag=1)

# Plot the differenced time series
plot.ts(differenced_ts,main = "differenced_ts")

result <- adf.test(differenced_ts)

# Print the test results
print(result)




acf(differenced_ts, lag.max = 100)
acf(differenced_ts, lag.max = 100, plot = FALSE)




pacf(differenced_ts, lag.max = 100)
pacf(differenced_ts, lag.max = 100, plot = FALSE)




result <- adf.test(differenced_ts)

# Print the test results
print(result)





acf(differenced_ts, main="ACF Stationary ts")
pacf(differenced_ts,main="PACF Stationary ts")





#Building an ARIMA model
arima_model <- arima(differenced_ts,order = c(1,1,1))
arima_model


arima_model_2 <- arima(ts_data, order = c(0, 1, 1))
arima_model_2



## compare models


# Check residuals for Model 1
checkresiduals(arima_model)

# Check residuals for Model 2
checkresiduals(arima_model_2)


print("AIC MODEL 2")
AIC(arima_model)
AIC(arima_model)

print("BIC MODEL 2")
BIC(arima_model_2)
BIC(arima_model_2)



## algeibric format of the model

##Y(t) = c + ??1 \* Y(t-1) + ??(t)

## checking the accurancy of the model



# Define the length of the training set
train_length <- 0.8 * length(differenced_ts)  # 80% of the data for training

# Create the training set
train_data <- differenced_ts[1:train_length]

# Create the test set
test_data <- differenced_ts[(train_length + 1):length(differenced_ts)]



fcst_vals <- forecast(arima_model,h=length(test_data))
fcst_vals

png("forecast_vals.png")
plot(fcst_vals)
dev.off()

png("original ts.png")
plot.ts(differenced_ts)
dev.off();








## Spectral Analysis


# get residuals
residuals <- residuals(arima_model)

# Perform spectral analysis
spec <- spectrum(residuals)

# Plot the spectrum
plot(spec, main = "Spectral Analysis of ARIMA Model Residuals")






residuals <- residuals(arima_model)

# Plotting the residuals
plot(residuals)


print(class(test_data))
# Convert the forecast values to numeric
forecasts_numeric <- as.numeric(fcst_vals$mean)

# Calculate Mean Absolute Error (MAE)
mae <- mean(abs(forecasts_numeric - test_data))

# Calculate Root Mean Squared Error (RMSE)
rmse <- sqrt(mean((forecasts_numeric - test_data)^2))

# Calculate Mean Absolute Percentage Error (MAPE)
mape <- mean(abs((forecasts_numeric - test_data) / test_data)) * 100


# Print the calculated accuracy measures
cat("Mean Absolute Error (MAE):", mae, "\n")
cat("Root Mean Squared Error (RMSE):", rmse, "\n")
cat("Mean Absolute Percentage Error (MAPE):", mape, "%\n")



## Results

## Mean Absolute Error (MAE): 0.07033137 Root Mean Squared Error (RMSE): 0.09319505 Mean Absolute Percentage Error (MAPE): 121.2969% 
## The results above suggest that the model has good accurancy and can be used for forecasting

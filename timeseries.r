# Time Series
#Dataset_name: tsdata
#Student Number ID:x24207438
# installing all libraries and using  
library(forecast)
library(tseries)
library(readr)
library(forecast)
library(ggplot2)

# Loading time series data
tsdata <- read_csv("C:/Users/admin/Downloads/Statistics/ts_data/ts8.csv")
View(tsdata)
names(tsdata) # return all column names
str(tsdata) # gives us structure and types of data
head(tsdata) #gives first few row
summary(tsdata)

#Fitting time series
ts_data <- ts(tsdata$x)
summary(ts_data)

#Plotting time series
plot(ts_data,
     main="Time Series Plot",
     xlab="Time",
     ylab="Value")

#Stationarity check
adf_result <- adf.test(ts_data)
adf_result

# ACF and PACF plots
acf(ts_data, main="ACF Plot of Series", )
pacf(ts_data, main="PACF Plot of Series")

# Split the data
n <- length(ts_data)
train_size <- floor(0.8 * n)

train <- window(ts_data, end = train_size)
test  <- window(ts_data, start = train_size + 1)

length(train)
length(test)

h <- length(test)
#Fitting models 
#Mean Method
mean_mod <- meanf(train, h = length(test))

#Naive Method
naive_mod <- naive(train, h = length(test))

#Exponential Smoothing
ets_mod <- ets(train)
ets_fore <- forecast(ets_mod, h = length(test))

#ARIMA 
arima_mod <- auto.arima(train)
arima_mod
arima_fore <- forecast(arima_mod, h = length(test))
arima_fore


#Plotting Arima forecasts and ets forecast
autoplot(arima_fore) +
  ggtitle("ARIMA Forecast") +
  xlab("Time") + ylab("Value")

autoplot(ets_fore) +
  ggtitle("ETS Forecast") +
  xlab("Time") + ylab("Value")

# Accuracy of mean, naive, ets, arima
acc_mean  <- accuracy(mean_mod,  test)
acc_naive <- accuracy(naive_mod, test)
acc_ets   <- accuracy(ets_fore,  test)
acc_arima <- accuracy(arima_fore, test)

acc_mean
acc_naive
acc_ets
acc_arima


# Residuals diagnostics
checkresiduals(arima_mod)
# Shows residual ACF, histogram, and Ljung–Box test

# Extractting  residuals 
ets_residuals <- residuals(ets_mod)

# Histogram of ETS Residuals
hist(ets_residuals,
     main="Histogram of ETS Residuals",
     xlab="Residual Value",
     col="lightblue", 
     border="black")

# Q-Q Plot of ETS Residuals to check for normality
qqnorm(ets_residuals,
       main="Normal Q-Q Plot of ETS Residuals")
qqline(ets_residuals, col="red")



# Extractting residuals from the ARIMA model 
arima_residuals <- residuals(arima_mod)

# ACF Plot of ARIMA Residuals 
acf(arima_residuals,
    main="ACF Plot of ARIMA Residuals")


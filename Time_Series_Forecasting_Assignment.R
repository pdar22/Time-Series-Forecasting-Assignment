#Load the libraries
library(xlsx)
library(tseries)
library(forecast)
library(smooth)

#Import the data into R
setwd("/Users/pranavdar/Desktop/PGPBABI/Time Series Forecasting/TSF Material")
sales_ts <- read.xlsx("Sales Data for TS Assgn (2).xlsx",1)

#Take a look at the dataset
View(sales_ts)
str(sales_ts)

# Convert the data to a timeseries object
sales <- ts(sales_ts[,-1], start=c(2011,1), end=c(2017,5), frequency=12)
plot(sales)

#Moving Average
sales3 <- ma(sales, order=3)
sales9 <- ma(sales, order=9)
sales19 <- ma(sales, order=19)
sales25 <- ma(sales,order=25)
  
#Visualizing the moving averages with respect to the original data
ts.plot(sales, sales3, sales9, sales19, sales25, lty=c(1:5), col=c('black','red','blue', 'forest green','grey'))

#Decomposing, smoothing and plotting using the multiplicative type 
sales_mult <- ts(na.omit(sales25), frequency=12)
sales_decomp <- stl(sales_mult, s.window="periodic",allow.multiplicative.trend==TRUE)
deseasonal_cnt <- seasadj(sales_decomp)
plot(sales_decomp)
#Data has been smoothed

#Dickey-Fuller Test for stationarity
#Null hypothesis = Series is non-stationary
adf.test(sales_mult, alternative = "stationary")

#Series is not stationary so we need to transform it

#Performing Autocorrelation and Partial Autocorrelation
Acf(sales_mult,main='')
Pacf(sales_mult,main='')

#Differencing the series
count_d1 = diff(deseasonal_cnt, differences = 2) #differences=1 did not make the series stationary
plot(count_d1)
adf.test(count_d1, alternative = "stationary")

#Series is stationary with difference=2

#ACF and PACF for the Differenced Series
Acf(count_d1, main='ACF for Differenced Series')
Pacf(count_d1, main='PACF for Differenced Series')

#Split the time series into training and hold out samples
sales_train <- window(sales, start=c(2011,01), end=c(2015,12))
sales_test <- window(sales, start=c(2016,01), end=c(2017,05))

# Exponential Smoothing on the train data and MAPE calculation
sales_fit2 <- HoltWinters(sales_train, alpha=0.1, beta=0.1, gamma=0.2, seasonal="mult")
sales_fit2EX <- forecast(sales_fit2, 17)
plot(sales_fit2EX)

vec1 <- cbind(sales_test,sales_fit2EX$mean)
ts.plot(vec1, col=c("blue", "red"))
MAPE <- mean(abs(vec1[,1]-vec1[,2])/vec1[,1])
MAPE 

#Forecasting values from June 2017 to December 2018
sales_forecastEX <- HoltWinters(sales, alpha=0.1, beta=0.1, gamma=0.2, seasonal="mult")
sales_forecast <- forecast(sales_forecastEX, 19)
plot(sales_forecast,main="Final Forecast (June 2017-December 2018")

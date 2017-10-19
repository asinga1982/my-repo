#Time Series Analysis

#http://otexts.org/fpp2

library(forecast)
data(austres, AirPassengers)

#Plots
autoplot(austres) +
  ggtitle("Australian Residents") +
  xlab("Year") + ylab("Numbers (in thousands) of Australian residents")

#Seasonal Plot
ggseasonplot(austres, year.labels=TRUE, year.labels.left=TRUE) +
  ylab("Numbers (in thousands)") + ggtitle("Seasonal plot: Australian Residents")

# Polar Plot
ggseasonplot(austres, polar=TRUE) +
  ylab("Numbers (in thousands)") + ggtitle("Polar seasonal plot: Australian Residents")

# Sub Series Plot
ggsubseriesplot(austres) + ylab("$ million") +
  ggtitle("Seasonal subseries plot: antidiabetic drug sales")

#Extracting a section of series
res2 <- window(austres, start=1980)

#Lagplot
gglagplot(res2)


#Lag Plot
ggAcf(austres, lag.max = 50)

ggAcf(AirPassengers, lag.max = 50)

#Basic Forecasting Methods: (Mostly used for Bench Marking)
---------------------------
meanf(AirPassengers, h=20) #Mean of previous values
naive(AirPassengers, h=20) #Same as last value
snaive(AirPassengers,h=20) # Same as last season's value
plot(rwf(AirPassengers, h=50,drift = T)) # Drift to predict the trend

#Make time plots simple by removing variation due to calendar days, 
#population,
#Inflation
#Price Index
#CPI
#Log/Box-Cox Transformations

AP <- AirPassengers/monthdays(AirPassengers)
cbind(AirPassengers, AP) %>% autoplot(facet=T)

lambda <- BoxCox.lambda(AP)
autoplot(BoxCox(AP, lambda))

#Bias Adjustment
plot(rwf(AP,drift = T, lambda = lambda, biasadj = T))

#Residuals Should be:
#1. Uncorrelated
#2. Have mean zero
#3. Normaly distributed (nice to have)
#4. Have constant variation (nice to have)

#Prediction Interval is calculated using Normal Distribution

#Ljung-Box test for residuals, a large p-value indicates that Q* is not significant
checkresiduals(naive(AP))

#Doing Ljung Box test separatly
naive_model <- snaive(AP)
Box.test(naive_model$residuals, lag=10,fitdf=0, type="Lj")

#A forecast method that minimizes the MAE will lead to forecasts of the median, 
#while minimizing the RMSE will lead to forecasts of the mean

#Cross validation 
err <- tsCV(AP, rwf, drift=TRUE, h=1)
sqrt(mean(err^2, na.rm=TRUE))

#Outputs all Accuracy numbers
accuracy(naive_model)



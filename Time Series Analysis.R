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




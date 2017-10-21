#Time Series Analysis

#http://otexts.org/fpp2

library(forecast)
library(fpp2)
data(austres, AirPassengers)

#inear Regression in Time Series
summary(tslm(Consumption ~ Income+Unemployment+Production, data=uschange))

#Plotting Different Variables
uschange %>%
  as.data.frame %>%
  GGally::ggpairs()

#Fitting Trend and Categorical variables for Seasonality
beer2 <- window(ausbeer, start=1992)
fit.beer <- tslm(beer2 ~ trend + season)
summary(fit.beer)

#Visualize the fit
autoplot(beer2, series="Data") +
  forecast::autolayer(fitted(fit.beer), series="Fitted") +
  xlab("Year") + ylab("Megalitres") +
  ggtitle("Quarterly Beer Production Prediction")

#Vizualize the linear fit
cbind(Data=beer2, Fitted=fitted(fit.beer)) %>%
  as.data.frame %>%
  ggplot(aes(x=Data, y=Fitted, colour=as.factor(cycle(beer2)))) +
  geom_point() +
  ylab("Fitted") + xlab("Actual values") +
  ggtitle("Quarterly beer production") +
  scale_colour_brewer(palette="Dark2", name="Quarter") +
  geom_abline(intercept=0, slope=1)

#Fourier Series, max(K)=m/2, m=Seasonality
# it is useful when m is large (E.g: Weekly data)
fourier.beer <- tslm(beer2 ~ trend + fourier(beer2, K=2))
summary(fourier.beer)

#bizdays(ausbeer) can calculated no of trading days in the month/Quater

#Lagged variables can be used for ad campaigns, 
#Events can be represented as 0 and 1

#ACF of errors -> Model is usable but can be improved
#If Errors are normally distributed, prediction interval is easier to calculated
#Plot residuals against Predictors to see if there is a evidence of non-linear relationship
#Plot residuals agains fitted. If there is a trend then a transformation of the outcome is required

#Generates all the model selection parameters for a given model
CV(fourier.beer)

#Fitting exponential Trend
fit.exp <- tslm(marathon ~ trend, lambda = 0)

#Vizualizing the plot
autoplot(marathon) +
  forecast::autolayer(fitted(fit.exp), series="Exponential")


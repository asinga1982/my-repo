#Simple Expo Smoothing, Holt's Method, Holt Winter's, State Space Models and Model Selection
#http://otexts.org/fpp2/ch-expsmooth.html
library(forecast)
library(fpp2)

#Simple Exponential Smoothing, for time series which dont have trend or seasonality
#Parameters estimated are aplha (smoothing) and level, l0
fit <- ses(window(oil,start=1996))
accuracy(fit)
summary(fit)

autoplot(fit)

#Holt-Winters Method, for data with trends
#Parameters estimated are aplha (smoothing for level) and level for alpha l0, 
#beta (smoothing/slope for trend) and level for beta b0
air <- window(ausair, start=1990)
autoplot(holt(air,h=5))
summary(holt(air,h=5))

#Damped (phi between 0.8 and 0.98) Holt-Winters Method, for data with trends 
fit <- holt(air, damped=TRUE, h=15, phi=0.9)
summary(fit)

#Cross-Validation and Comparison:
e1 <- tsCV(livestock, ses, h=1)
e2 <- tsCV(livestock, holt, h=1)
e3 <- tsCV(livestock, holt, damped=TRUE, h=1)

# Compare MSE:
mean(e1^2, na.rm=TRUE)
mean(e2^2, na.rm=TRUE)
mean(e3^2, na.rm=TRUE)

# Compare MAE:
mean(abs(e1), na.rm=TRUE)
mean(abs(e2), na.rm=TRUE)
mean(abs(e3), na.rm=TRUE)

#Holt Winter's Trend and Seasonality
#Parameters estimated are aplha (smoothing for level) and level for alpha l0, 
#beta (smoothing/slope for trend) and level for beta, b0
#gamma (smoothing/slope for seasonality) and level s0
aust <- window(austourists,start=2005)

fit1 <- hw(aust,seasonal="additive")
fit2 <- hw(aust,seasonal="multiplicative")

autoplot(aust) +
  forecast::autolayer(fit1, PI=FALSE, series="HW additive forecasts") +
  forecast::autolayer(fit2, PI=FALSE, series="HW multiplicative forecasts") +
  xlab("Year") + ylab("International visitor night in Australia (millions)") +
  guides(colour=guide_legend(title="Forecast"))

#The small value of ?? for the multiplicative model means that the seasonal component hardly changes over time. 
#The small value of ??* for the additive model means the slope component hardly changes over time (check the vertical scale).

#Damping can be used with Holt Winter's method as well:
hw(aust, damped=TRUE, seasonal="multiplicative")

#Space State Moels are used to get prediction intervals
#Errors can be additive or multiplicative

#ets() can do automatic model selection
fit <- ets(aust)

#Forecast errors
fc_err <- residuals(fit, type = "response")

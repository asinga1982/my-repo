#http://otexts.org/fpp2/ch-expsmooth.html
library(forecast)
library(fpp2)

#Simple Exponential Smoothing, for time series which dont have trend or seasonality
#Parameters estimated are aplha (smoothing) and level, l0
fit <- ses(window(oil,start=1996))
accuracy(fit)
summary(fit)

autoplot(fit)


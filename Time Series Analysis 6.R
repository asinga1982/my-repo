#Dynamic Regression
library(fpp2)

#Add Inome as one of the predictors. Apply ARIMA on the residual of the regreession erors
fit <- auto.arima(uschange[,"Consumption"], xreg=uschange[,"Income"])

#Forecasting, assuming that predictor is accurate. 
fcast <- forecast(fit, xreg=rep(mean(uschange[,2]),8))

#Prediction interval does not consider uncertainity of the predictor.
autoplot(fcast) + xlab("Year") +
  ylab("Percentage change")

#Detriministic trend model -> assumes no differencing is required and models regression 
#errors as ARMA. While Stochastic trend model assumes differencing is required. and models
#errors as ARIMA.

#Deterministic Trend
trend <- seq_along(austa)
fit1 <- auto.arima(austa, d=0, xreg=trend)

#stochastic Trend 
fit2 <- auto.arima(austa, d=1)

#Plot time-series and prediction intervals
fc1 <- forecast(fit1, xreg=data.frame(trend=length(austa)+1:10))
fc2 <- forecast(fit2, h=10)
autoplot(austa) +
  forecast::autolayer(fc2, series="Stochastic trend") +
  forecast::autolayer(fc1, series="Deterministic trend") +
  ggtitle("Forecasts from deterministic and stochastic trend models") +
  xlab("Year") + ylab("Visitors to Australia (millions)") +
  guides(colour=guide_legend(title="Forecast"))


#Detrministic trend model assumes that trend will not change overtime. For longer term 
#prediction, stochastic trend model will be better





#Models with Lagged Predictors
autoplot(insurance, facets=TRUE) +
  xlab("Year") + ylab("") +
  ggtitle("Insurance advertising and quotations")

# Lagged predictors. Test 0, 1, 2 or 3 lags.
Advert <- cbind(
  AdLag0 = insurance[,"TV.advert"],
  AdLag1 = lag(insurance[,"TV.advert"],-1),
  AdLag2 = lag(insurance[,"TV.advert"],-2),
  AdLag3 = lag(insurance[,"TV.advert"],-3))[1:NROW(insurance),]

# Choose optimal lag length for advertising based on AICc
# Restrict data so models use same fitting period
fit1 <- auto.arima(insurance[4:40,1], xreg=Advert[4:40,1], d=0)
fit2 <- auto.arima(insurance[4:40,1], xreg=Advert[4:40,1:2], d=0)
fit3 <- auto.arima(insurance[4:40,1], xreg=Advert[4:40,1:3], d=0)
fit4 <- auto.arima(insurance[4:40,1], xreg=Advert[4:40,1:4], d=0)

# Best model fitted to all data (based on AICc)
# Refit using all data
fit <- auto.arima(insurance[,1], xreg=Advert[,1:2], d=0)
checkresiduals(fit)

#FORECASTING
fc8 <- forecast(fit, h=20,
                xreg=cbind(AdLag0=rep(8,20), AdLag1=c(Advert[40,1],rep(8,19))))

autoplot(fc8) + ylab("Quotes") +
  ggtitle("Forecast quotes with future advertising set to 8")



fit1 <- hw(myts,seasonal = "mult",h=50)
fit2 <- hw(myts,seasonal = "mult",h=50, damped = T)
#fit3 <- holt(eggs,h=10, damped = T, lambda = T)
#fit3 <- hw(eggs,h=10, damped = F, lambda = F, seasonal = "multiplicative")

autoplot(myts) +
  forecast::autolayer(fit1, PI=FALSE, series="Default") +
  forecast::autolayer(fit2, PI=FALSE, series="Damped") +
#  forecast::autolayer(fit3, PI=FALSE, series="Damped with box-cox") +
    xlab("Year") + ylab("Eggs Price") +
  guides(colour=guide_legend(title="Forecast"))

#-----------------------------------------------------------------------
train <- window(mytimeseries, end=c(2010,12))
test <- window(mytimeseries, start=c(2011,1))

df1 <- data.frame(train)
df2 <- data.frame(test)
newtrain <- ts(df1[,1], start = c(1982,4), frequency = 12)
df2 <- data.frame(test)
newtest <- ts(df2[,1], start = c(2011,1), frequency = 12)

lambda <- BoxCox.lambda(newtrain)

train_tx <- BoxCox(newtrain, lambda)

fit_stl <- stl(x=train_tx, s.window = "periodic", t.window = 13)
autoplot(seasadj(fit_stl))
 
train_seasadj <- seasadj(fit_stl)

fit_ets <- ets(train_seasadj)

#Pred
ets_fc <- forecast(fit_ets, h=36)
ets_df <- data.frame(ets_fc$mean)

s <- seasonal(fit_stl)

s <- data.frame(s)
seas_val <- s[10:21,1]

seas_effect <- rep(seas_val,3)
seas_adj_fc <- ets_df + seas_val

fc <- InvBoxCox(seas_adj_fc,lambda = lambda)
fs_ts <- ts(fc[,1],start=c(2011,1), frequency = 12)

err1 <- rmse(test, fs_ts)
err1
autoplot(test, series = "test data") + forecast::autolayer(fs_ts, series="predicted")

hw.fit <- hw(train,h=36,damped = F)

err2 <- rmse(test,hw.fit$mean)
err2
#----------------------------------------------------------
autoplot(ukcars)

stl_fit <- stl(ukcars)
autoplot(stl_fit)

seas_adj <- seasadj(stl_fit)

autoplot(ukcars) +forecast::autolayer(seasadj(stl_fit), series="seasadj")

fc1 <- stlf(ukcars, etsmodel="AAN", damped=T, h=8)
summary(fc1)

hw_fit <- holt(seas_adj, h=8,damped = F)
summary(hw_fit)

ets_fit <- ets(ukcars)
#-----------------------------------------------------------------
autoplot(visitors)

train <- window(visitors, end=c(2003,4))
test <- window(visitors, start=c(2003,5))

hw_fc <- hw(train, h=24, seasonal = "multi", damped = T)

err_hw <- rmse(hw_fc$mean, test)
err_hw

ets_fit <- ets(train, h=24)
err_ets <- rmse(ets_fit$mean, test)
err_ets

lambda <- BoxCox.lambda(train)

ets_add <- ets(train, model="ZZZ", lambda = lambda, additive.only = T)
ets_addfc <- forecast(ets_add, h=24)
err_etsadd <- rmse(ets_addfc$mean, test)
err_etsadd

snaive_fc <- snaive(train, h=24)
err_snaive <- rmse(snaive_fc$mean, test)
err_snaive

stlf_fit <- stlf(train, s.window = "periodic",lambda = lambda, 
                 etsmodel = "ZZN", h=24)
err_stlf <- rmse(stlf_fit$mean, test)
err_stlf

sum(abs(tsCV(ukcars, snaive)), na.rm = T)
sum(abs(tsCV(ukcars, stlf,lambda=lambda, s.window="periodic",etsmodel = "ZZN" )), na.rm = T)
sum(abs(
  tsCV(ukcars, ets,model="ZZZ", lambda = lambda, additive.only = T )), na.rm = T)


sum(abs(tsCV(ukcars, hw,seasonal = "multi", damped = T)), na.rm = T)

crs <- data.frame(ukcars)
crs.ts <- ts(crs[,1], start=c(1977,1), frequency = 4)

far2 <- function(x, h){forecast(ets_add(x, h=h))}
e <- tsCV(crs.ts, far2, h=1)
e
#-----------------------------------------------------------------------

autoplot(dole)
ggseasonplot(dole, year.labels = T)

autoplot(decompose(dole))

train <- window(dole, end=c(1989,7))
test <- window(dole, start=c(1989,8))

autoplot(train)
autoplot(test)

lambda <- BoxCox.lambda(train)
lambda

ets.model <- ets(train)
ets.model

ets_fc <- forecast(ets.model, h=36)

rmse(test, ets_fc$mean)
#304076
checkresiduals(ets.model)
#failed

sn.fc <- snaive(train, h=36)

rmse(test, sn.fc$mean)
#240830
checkresiduals(sn.fc)
#failed

stlf.fc <- stlf(train,etsmodel="ZZN", lambda = lambda, h=36, robust = T, 
                s.window = 17)
summary(stlf.fc)

rmse(test, stlf.fc$mean)
#268090
checkresiduals(stlf.fc)
#failed, but better than earlier 2
#---------------------------------------------------------------
autoplot(elecequip)
ggseasonplot(elecequip)
ggtsdisplay(elecequip)
lambda <- BoxCox.lambda(elecequip)

#Get seasonaly adjusted series

stl.fit <- stl(elecequip, s.window = "p")
seasadj.data <- seasadj(stl.fit) 

#Apply Box-Cox TX
#mod.data <- BoxCox(elecequip,lambda = lambda)
#autoplot(mod.data)

ndiffs(seasadj.data)
#1
nsdiffs(seasadj.data)
#0

x <- seasadj.data
ns <- nsdiffs(x)
if(ns > 0) {
  xstar <- diff(x, lag=frequency(x), differences=ns) 
} else {
  xstar <- x 
}

nd <- ndiffs(xstar)
if(nd > 0)
  xstar <- diff(xstar, differences=nd)

#Plots ACF and PACF
ggtsdisplay(xstar)

AR.model310 <- Arima(seasadj.data,order=c(3,1,0))
checkresiduals(AR.model310)
summary(AR.model310)

AR.model311 <- Arima(seasadj.data,order=c(3,1,1))
checkresiduals(AR.model311)
summary(AR.model311)

auto.arima(seasadj.data,approximation = F)


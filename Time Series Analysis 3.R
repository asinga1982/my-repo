#Time Series Analysis - Decomposition using Moving Avg & X11 

#http://otexts.org/fpp2

library(forecast)
library(fpp2)

#Moving Average of order 5, to determine trend-cycle
#Higher order means smoother trend
mv_avg <- ma(elecsales, 5)

autoplot(elecsales, series ="original") +
  forecast::autolayer(mv_avg, series="Moving Avg")

#if the seasonal period is even and of order m,use a 2×m-MA to estimate the trend-cycle. 
#if the seasonal period is odd and of order m, use a m-MA to estimate the trend-cycle.
#Other choices will result in trend being contaminated by seasonality

mv_avg <- ma(AirPassengers, 12, centre = T)
autoplot(AirPassengers, series ="original") +
  forecast::autolayer(mv_avg, series="Moving Avg")

#Weighted Moving Averages result in smoother trend-cycle.

#Classical Approach:
#--------------------
#Seasonal component for additive series is done by - 
#1. Removing the trend from the series
#2. For seasonal component for each season, simply average the detrended values for that season. 
#For example, with monthly data, the seasonal component for March is the average of all the detrended March values in the data. 
#These seasonal component values are then adjusted to ensure that they add to zero

fit <- decompose(elecequip, type="multiplicative")
autoplot(fit) + xlab("Year") +
  ggtitle("Classical multiplicative decomposition of electrical equipment index")

#Issues:
# This menthod is unable to capture changes in seasonality over years
# Trend and Remainder are not available for some of the initial and last values
# Trend is too smoooth for rapid rise and fall thus leaks values into remainder
# Not robust to unusual values

#X11 Decomposition, does a better job than the classical approach.
library(seasonal)

fit <- seas(elecequip, x11="")
autoplot(fit) +
  ggtitle("X11 decomposition of electrical equipment index")

#Individual X11 Plots
autoplot(trendcycle(fit))
autoplot(seasadj(fit))

### JOSEPH CLARO - TIME SERIES ANALYSIS PROJECT
### Avg. Monthly Wind Speeds - Valentia Island Weather Station, 2005-2025

# alt + '-' : <- 
install.packages("this.path")
install.packages("zoo")

library(TSA)
library(zoo)

setwd(this.path::here())

windSpeed <- read.csv("windSpeed.csv")
windSpeed85 <- read.csv("windSpeed_Last15%Removed.csv")

#Full time series
dataFull <- windSpeed$mean.wind.speed
windFull <- ts(dataFull, 
         freq = 12, start = c(2005,1))

#Shortened time series (removed last 15%)
data <-  windSpeed85$mean.wind.speed
wind <- ts(data,
        freq = 12, start = c(2005,1))

# Series plot - original
plot(wind,
     ylab='Average Wind Speed (knots)',main='brat', xlab='Year',type='l')

#Seasonal Boxplot 
boxplot(wind~season(wind),xlab="Month", ylab='Average Wind Speed (knots)')
#suggests data roughly seasonal, winter more variable

#Exploratory decomposition
plot(decompose(wind))
# Fails to identify strong seasonal trend (axis ranges small)

#ACF plot
acf(wind, lag.max=240)
# Decaying oscillation - Strong autocorrelations at lags 6,12,18...
# Implies the need for seasonal differencing

pacf(wind, lag.max=60)
# Significant (non-coincidental) autocorrelations cut off at lag-12

#S.Diff'd Series plot
d12wind <- diff(wind, lag=12)
plot(d12wind,
     ylab='Seasonal Wind Diff.', xlab='Year',type='l')


#S.Diff'd Series ACF
acf(d12wind, lag.max = 240)

#S.Diff'd Series Partial ACF
pacf(d12wind, lag.max = 240)

# Test for stationarity
adf.test(wind)
adf.test(d12wind)


# Model Fitting - EACF (seasonally differenced series)
eacf(wind)
# Hard to read, supports need to seasonally difference as per ACF

eacf(d12wind)
# Suggests a seasonal model with a MA(12) component

# Model Fitting - Minimising BIC
d12windsubs <- armasubsets(d12wind, nar=18, nma=18)

plot(d12windsubs)
# Potential models (considering parsimony// BIC +/-2 insignificant diff.):
# ARMA(12,9) x (0,1,0)_12
# ARMA(12,8) x (0,1,0)_12

windmod1 <- arima(wind, order=c(12,0,9),
                  seasonal=list(order=c(0,1,0), period=12))

windmod2 <- arima(wind, order=c(12,0,8),
                  seasonal=list(order=c(0,1,0), period=12))

print(windmod1) # ARMA(12,9) x (0,1,0)_12
print(windmod2) # ARMA(12,8) x (0,1,0)_12

tsdiag(windmod1)
tsdiag(windmod2)

#Residuals
resid1 <- rstandard(windmod1)
resid2 <- rstandard(windmod2)

fitmod1 <- fitted(windmod1)
fitmod2 <- fitted(windmod2)

#MODEL 1 RES.
hist(resid1) #follows normal dist.
plot(as.vector(fitmod1), as.vector(resid1))

qqnorm(resid1); qqline(resid1); #
shapiro.test(resid1) #

#MODEL 2 RES.
hist(resid2) #follows normal dist.
plot(as.vector(fitmod2), as.vector(resid2))

qqnorm(resid2); qqline(resid2); #
shapiro.test(resid2) #

#MODEL 1 Plot
plot(windmod1, n.ahead=37, xlim=c(2020,2025),
     main="Model 1: ARMA(12,9) x (0,1,0)_12",
     ylab='Average Monthly Wind Speed (Knots)', xlab='Year')
lines(windFull, col='#BADBED')

#MODEL 2 Plot
plot(windmod2, n.ahead=37, xlim=c(2020,2025),
     main="Model 1: ARMA(12,8) x (0,1,0)_12",
     ylab='Average Monthly Wind Speed (Knots)', xlab='Year')
lines(windFull, col='blue')





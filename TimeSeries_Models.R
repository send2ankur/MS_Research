library(dplyr)
library(forecast)
library(tseries)
library(dataPreparation)

# Import rds objets for delhi and ghaziabad hourly files
delpb <- readRDS("df_delpb_hourly.rds")
gha <- readRDS("df_gha_hourly.rds")


###########################################
# Delhi
###########################################

############# Training 

# Create time series object
train_ts <- delpb %>% 
  filter(Timeline >= '2018-01-01 00:00:00' & Timeline <= '2019-12-31 23:00:00') %>%
  select (PM2.5) %>%
  dplyr::rename(pm25_0 = PM2.5) %>%
  ts(frequency = 24)

plot(decompose(train_ts, type="additive"))


# ACF PACF Plots
par(mfrow=c(1,2))
acf(train_ts)
pacf(train_ts)
# Ist order differencing
par(mfrow=c(1,2))
acf(diff(train_ts))
pacf(diff(train_ts))


# Prepare xreg
xreg_to_scale <- delpb %>%
  filter(Timeline >= '2018-01-01 00:00:00' & Timeline <= '2019-12-31 23:00:00') %>%
  subset(select = c(precipIntensity,precipProbability,apparentTemperature,dewPoint,pressure,
                    windSpeed,windGust,cloudCover,uvIndex,visibility))

arimax_scales <- build_scales(xreg_to_scale, verbose = TRUE)
saveRDS(arimax_scales,"arimax_scales_delpb.rds")
arimax_scales <- readRDS("arimax_scales_delpb.rds")
xreg_scale <- fastScale(xreg_to_scale, scales = arimax_scales, verbose = TRUE)
xreg_train <- as.matrix(xreg_scale)

# Build ARIMAX
arimax_delpb <- forecast::auto.arima(train_ts,seasonal=TRUE,trace=TRUE,xreg=xreg_train,
                                     parallel=TRUE)
# Regression with ARIMA(3,1,1)(0,0,2)[24]
# Save rds object
saveRDS(arimax_delpb,"arimax_delpb.rds")


tsdiag(arimax_delpb)
plot(arimax_delpb$x, col="black")
lines(fitted(arimax_delpb), col="red")

#Again, let's check if the residual series is white noise
resi_auto_arima <- train_ts - fitted(arimax_delpb)
adf.test(resi_auto_arima,alternative = "stationary")
# Dickey-Fuller = -21.312, Lag order = 25, p-value = 0.01
kpss.test(resi_auto_arima)
# KPSS Level = 0.060765, Truncation lag parameter = 14, p-value = 0.1


############# Test

test <- delpb %>%
  filter(Timeline >= '2020-01-01 00:00:00')

# Prepare xreg
xregt_to_scale <- delpb %>%
  filter(Timeline >= '2020-01-01 00:00:00') %>%
  subset(select = c(precipIntensity,precipProbability,apparentTemperature,dewPoint,pressure,
                    windSpeed,windGust,cloudCover,uvIndex,visibility))
xregt_scale <- fastScale(xregt_to_scale, scales = arimax_scales, verbose = TRUE)
xreg_test <- as.matrix(xregt_scale)

# Model evaluation 96 hours
fcast_auto_arima <- forecast(arimax_delpb, xreg=xreg_test[c(1:360),], n.ahead = 360)
accuracy(fcast_auto_arima$fitted[1:360],delpb_test$PM2.5[1:360])
#                  ME     RMSE      MAE       MPE     MAPE
#Test set    -93.16557 155.2201 123.8407 -110.1408 120.9653

arimax_delpb_eval <- data.frame(Timeline=as.POSIXct(delpb_test$Timeline[1:360]), 
                                Forecast=fcast_auto_arima$fitted[1:360], 
                                Actual=delpb_test$PM2.5[1:360])
# save rds object
saveRDS(arimax_delpb_eval,"C:/Users/send2/Documents/Ankur/MSc/Temp/rdsObject/arimax_delpb_eval.rds")

###########################################
# Ghaziabad
###########################################

############# Training 

# Create time series object
train_ts <- gha %>% 
  filter(Timeline >= '2018-01-01 00:00:00' & Timeline <= '2019-12-31 23:00:00') %>%
  select (PM2.5) %>%
  dplyr::rename(pm25_0 = PM2.5) %>%
  ts(frequency = 24)

plot(decompose(train_ts, type="additive"))

# ACF PACF Plots
par(mfrow=c(1,2))
acf(train_ts)
pacf(train_ts)
# Ist order differencing
par(mfrow=c(1,2))
acf(diff(train_ts))
pacf(diff(train_ts))


# Prepare xreg
xreg_to_scale <- gha %>%
  filter(Timeline >= '2018-01-01 00:00:00' & Timeline <= '2019-12-31 23:00:00') %>%
  subset(select = c(precipIntensity,precipProbability,apparentTemperature,dewPoint,pressure,
                    windSpeed,windGust,cloudCover,uvIndex,visibility))

arimax_scales <- build_scales(xreg_to_scale, verbose = TRUE)
saveRDS(arimax_scales,"arimax_scales_gha.rds")
xreg_scale <- fastScale(xreg_to_scale, scales = arimax_scales, verbose = TRUE)
xreg_train <- as.matrix(xreg_scale)

# Build ARIMAX
arimax_gha <- forecast::auto.arima(train_ts,seasonal=TRUE,trace=TRUE,xreg=xreg_train)
# Best model: Regression with ARIMA(2,1,2)(1,0,0)[24]
saveRDS(arimax_gha,"arimax_gha.rds")

tsdiag(arimax_gha)
plot(arimax_gha$x, col="black")
lines(fitted(arimax_gha), col="red")

#Again, let's check if the residual series is white noise
resi_auto_arima <- train_ts - fitted(arimax_gha)
adf.test(resi_auto_arima,alternative = "stationary")
# Dickey-Fuller = -20.92, Lag order = 25, p-value = 0.01
kpss.test(resi_auto_arima)
# KPSS Level = 0.035704, Truncation lag parameter = 14, p-value = 0.1

############# Test

gha_test <- gha %>%
  filter(Timeline >= '2020-01-01 00:00:00')

# Prepare xreg
xregt_to_scale <- gha %>%
  filter(Timeline >= '2020-01-01 00:00:00') %>%
  subset(select = c(precipIntensity,precipProbability,apparentTemperature,dewPoint,pressure,
                    windSpeed,windGust,cloudCover,uvIndex,visibility))
xregt_scale <- fastScale(xregt_to_scale, scales = arimax_scales, verbose = TRUE)
xreg_test <- as.matrix(xregt_scale)

#Also, let's evaluate the model using MAPE
fcast_auto_arima <- forecast(arimax_gha, xreg=xreg_test[c(1:360),], n.ahead = 360)
accuracy(fcast_auto_arima$fitted[1:360],gha_test$PM2.5[1:360])
#                  ME     RMSE      MAE       MPE     MAPE
#Test set    -95.699 158.0659 125.8172 -84.58171 94.25856

arimax_gha_eval <- data.frame(Timeline=as.POSIXct(gha_test$Timeline[1:360]), 
                              Forecast=fcast_auto_arima$fitted[1:360], 
                              Actual=gha_test$PM2.5[1:360])
saveRDS(arimax_gha_eval,"arimax_gha_eval.rds")





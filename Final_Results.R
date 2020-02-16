library(dplyr)
library(ggplot2)
## ggplot theme setting
theme_set(theme_bw())
library(grid)
library(gridExtra)


############################################################
### FIT COMPARISON
############################################################

lmw019_eval <- readRDS("lmw019_eval.rds")
svmw01_eval <- readRDS("svmw01_eval.rds")
rfw02_eval <- readRDS("rfw02_eval.rds")
xgbw02_eval <- readRDS("xgbw02_eval.rds")
arimax_delpb_eval <-  readRDS("arimax_delpb_eval.rds")
arimax_gha_eval <-  readRDS("arimax_gha_eval.rds")

model_eval <- lmw019_eval %>%
  dplyr::rename(mlr_forecast = Forecast) %>%
  dplyr::mutate(svm_forecast = svmw01_eval[,c(2)]) %>%
  dplyr::mutate(rf_forecast = rfw02_eval[,c(2)]) %>%
  dplyr::mutate(xgb_forecast = xgbw02_eval[,c(2)]) %>%
  select(Timeline,Actual,mlr_forecast,svm_forecast,rf_forecast,xgb_forecast)

## Delhi 
delpb_eval <- model_eval[c(1:360),] %>%
  dplyr::mutate(arimax_forecast = arimax_delpb_eval[,c(2)] )
gha_eval <- model_eval[c(738:1097),] %>%
  dplyr::mutate(arimax_forecast = arimax_gha_eval[,c(2)] )


# ARIMAX
Plot1 <- ggplot(delpb_eval, aes(Timeline)) + 
  geom_point(aes(y = Actual, colour = "Observed")) +
  geom_point(aes(y = arimax_forecast, colour = "arimax_forecast")) +
  geom_line(aes(y = Actual, colour = "Observed")) + 
  geom_line(aes(y = arimax_forecast, colour = "arimax_forecast")) +
  scale_color_manual(name="", values=c('#999999','#E69F00')) +
  labs(x = "", y = expression(PM[2.]~concentration), title = "Delhi Forecast")

Plot2 <- ggplot(gha_eval, aes(Timeline)) + 
  geom_point(aes(y = Actual, colour = "Observed")) +
  geom_point(aes(y = arimax_forecast, colour = "arimax_forecast")) +
  geom_line(aes(y = Actual, colour = "Observed")) + 
  geom_line(aes(y = arimax_forecast, colour = "arimax_forecast")) +
  scale_color_manual(name="", values=c('#999999','#E69F00')) +
  labs(x = "", y = expression(PM[2.]~concentration), title = "Ghaziabad Forecast")

grid.arrange(Plot1,Plot2,
             nrow = 2, ncol =1,
             top = expression(PM[2.5]~hourly~forecast~with~ARIMAX~Model))

# MLR
Plot1 <- ggplot(delpb_eval, aes(Timeline)) + 
  geom_point(aes(y = Actual, colour = "Observed")) +
  geom_point(aes(y = mlr_forecast, colour = "mlr_forecast")) +
  geom_line(aes(y = Actual, colour = "Observed")) + 
  geom_line(aes(y = mlr_forecast, colour = "mlr_forecast")) +
  scale_color_manual(name="", values=c('#999999','#E69F00')) +
  labs(x = "", y = expression(PM[2.]~concentration), title = "Delhi Forecast")

Plot2 <- ggplot(gha_eval, aes(Timeline)) + 
  geom_point(aes(y = Actual, colour = "Observed")) +
  geom_point(aes(y = mlr_forecast, colour = "mlr_forecast")) +
  geom_line(aes(y = Actual, colour = "Observed")) + 
  geom_line(aes(y = mlr_forecast, colour = "mlr_forecast")) +
  scale_color_manual(name="", values=c('#999999','#E69F00')) +
  labs(x = "", y = expression(PM[2.]~concentration), title = "Ghaziabad Forecast")

grid.arrange(Plot1,Plot2,
             nrow = 2, ncol =1,
             top = expression(PM[2.5]~hourly~forecast~with~MLR~Model))

# SVM
Plot1 <- ggplot(delpb_eval, aes(Timeline)) + 
  geom_point(aes(y = Actual, colour = "Observed")) +
  geom_point(aes(y = svm_forecast, colour = "svm_forecast")) +
  geom_line(aes(y = Actual, colour = "Observed")) + 
  geom_line(aes(y = svm_forecast, colour = "svm_forecast")) +
  scale_color_manual(name="", values=c('#999999','#E69F00')) +
  labs(x = "", y = expression(PM[2.]~concentration), title = "Delhi Forecast")

Plot2 <- ggplot(gha_eval, aes(Timeline)) + 
  geom_point(aes(y = Actual, colour = "Observed")) +
  geom_point(aes(y = svm_forecast, colour = "svm_forecast")) +
  geom_line(aes(y = Actual, colour = "Observed")) + 
  geom_line(aes(y = svm_forecast, colour = "svm_forecast")) +
  scale_color_manual(name="", values=c('#999999','#E69F00')) +
  labs(x = "", y = expression(PM[2.]~concentration), title = "Ghaziabad Forecast")

grid.arrange(Plot1,Plot2,
             nrow = 2, ncol =1,
             top = expression(PM[2.5]~hourly~forecast~with~SVM~Model))

# RF
Plot1 <- ggplot(delpb_eval, aes(Timeline)) + 
  geom_point(aes(y = Actual, colour = "Observed")) +
  geom_point(aes(y = rf_forecast, colour = "rf_forecast")) +
  geom_line(aes(y = Actual, colour = "Observed")) + 
  geom_line(aes(y = rf_forecast, colour = "rf_forecast")) +
  scale_color_manual(name="", values=c('#999999','#E69F00')) +
  labs(x = "", y = expression(PM[2.]~concentration), title = "Delhi Forecast")

Plot2 <- ggplot(gha_eval, aes(Timeline)) + 
  geom_point(aes(y = Actual, colour = "Observed")) +
  geom_point(aes(y = rf_forecast, colour = "rf_forecast")) +
  geom_line(aes(y = Actual, colour = "Observed")) + 
  geom_line(aes(y = rf_forecast, colour = "rf_forecast")) +
  scale_color_manual(name="", values=c('#999999','#E69F00')) +
  labs(x = "", y = expression(PM[2.]~concentration), title = "Ghaziabad Forecast")

grid.arrange(Plot1,Plot2,
             nrow = 2, ncol =1,
             top = expression(PM[2.5]~hourly~forecast~with~RF~Model))

# XGB
Plot1 <- ggplot(delpb_eval, aes(Timeline)) + 
  geom_point(aes(y = Actual, colour = "Observed")) +
  geom_point(aes(y = xgb_forecast, colour = "xgb_forecast")) +
  geom_line(aes(y = Actual, colour = "Observed")) + 
  geom_line(aes(y = xgb_forecast, colour = "xgb_forecast")) +
  scale_color_manual(name="", values=c('#999999','#E69F00')) +
  labs(x = "", y = expression(PM[2.]~concentration), title = "Delhi Forecast")

Plot2 <- ggplot(gha_eval, aes(Timeline)) + 
  geom_point(aes(y = Actual, colour = "Observed")) +
  geom_point(aes(y = xgb_forecast, colour = "xgb_forecast")) +
  geom_line(aes(y = Actual, colour = "Observed")) + 
  geom_line(aes(y = xgb_forecast, colour = "xgb_forecast")) +
  scale_color_manual(name="", values=c('#999999','#E69F00')) +
  labs(x = "", y = expression(PM[2.]~concentration), title = "Ghaziabad Forecast")

grid.arrange(Plot1,Plot2,
             nrow = 2, ncol =1,
             top = expression(PM[2.5]~hourly~forecast~with~XGBoost~Model))


# Best model in terms of RMSE
Plot1 <- ggplot(delpb_eval, aes(Timeline)) + 
  geom_point(aes(y = Actual, colour = "Observed")) +
  geom_point(aes(y = svm_forecast, colour = "svm_forecast")) +
  geom_line(aes(y = Actual, colour = "Observed")) + 
  geom_line(aes(y = svm_forecast, colour = "svm_forecast")) +
  scale_color_manual(name="", values=c('#999999','#E69F00')) +
  labs(x = "", y = expression(PM[2.]~concentration), title = "Delhi SVM Forecast")

Plot2 <- ggplot(gha_eval, aes(Timeline)) + 
  geom_point(aes(y = Actual, colour = "Observed")) +
  geom_point(aes(y = rf_forecast, colour = "rf_forecast")) +
  geom_line(aes(y = Actual, colour = "Observed")) + 
  geom_line(aes(y = rf_forecast, colour = "rf_forecast")) +
  scale_color_manual(name="", values=c('#999999','#E69F00')) +
  labs(x = "", y = expression(PM[2.]~concentration), title = "Ghaziabad RF Forecast")

grid.arrange(Plot1,Plot2,
             nrow = 2, ncol =1,
             top = expression(PM[2.5]~hourly~forecast~best~Model))

############################################################
### PREDICT AQI
############################################################

### Delhi
delpb_eval_aqi <- delpb_eval %>%
  mutate(Date = as.Date(as.character(Timeline))) %>%
  group_by(Date) %>%
  summarise_all("mean") %>%
  mutate(actual_si =ifelse((Actual<=30),Actual*50/30,
                         ifelse((Actual>30 & Actual<=60),50+(Actual-30)*50/30,
                                ifelse((Actual>60 & Actual<=90),100+(Actual-60)*100/30,
                                       ifelse((Actual>90 & Actual<=120),200+(Actual-90)*(100/30),
                                              ifelse((Actual>120 & Actual<=250),300+(Actual-120)*(100/130),
                                                     ifelse((Actual>250),400+(Actual-250)*(100/130),NA))))))) %>%
  mutate(svm_si =ifelse((svm_forecast<=30),svm_forecast*50/30,
                           ifelse((svm_forecast>30 & svm_forecast<=60),50+(svm_forecast-30)*50/30,
                                  ifelse((svm_forecast>60 & svm_forecast<=90),100+(svm_forecast-60)*100/30,
                                         ifelse((svm_forecast>90 & svm_forecast<=120),200+(svm_forecast-90)*(100/30),
                                                ifelse((svm_forecast>120 & svm_forecast<=250),300+(svm_forecast-120)*(100/130),
                                                       ifelse((svm_forecast>250),400+(svm_forecast-250)*(100/130),NA))))))) %>%
  mutate(actual_aqicat = ifelse((actual_si<=50),"Good",
                             ifelse((actual_si>50 & actual_si<=100),"Satisfactory",
                                    ifelse((actual_si>100 & actual_si<=200),"Moderately Polluted",
                                           ifelse((actual_si>200 & actual_si<=300),"Poor",
                                                  ifelse((actual_si>300 & actual_si<=400),"Very Poor",
                                                         ifelse((actual_si>400),"Severe",NA)))))))  %>%
  mutate(svm_aqicat = ifelse((svm_si<=50),"Good",
                                ifelse((svm_si>50 & svm_si<=100),"Satisfactory",
                                       ifelse((svm_si>100 & svm_si<=200),"Moderately Polluted",
                                              ifelse((svm_si>200 & svm_si<=300),"Poor",
                                                     ifelse((svm_si>300 & svm_si<=400),"Very Poor",
                                                            ifelse((svm_si>400),"Severe",NA)))))))  %>%
  select(Date,actual_aqicat,svm_aqicat) %>%
  rename(Delhi.Observed = actual_aqicat)%>%
  rename(Delhi.Forecasted = svm_aqicat)

write.csv(delpb_eval_aqi,"C:/Users/send2/Documents/Ankur/MSc/Temp/dataFile/del_aqi_cat.csv")

### Ghaziabad
gha_eval_aqi <- gha_eval %>%
  mutate(Date = as.Date(as.character(Timeline))) %>%
  group_by(Date) %>%
  summarise_all("mean") %>%
  mutate(actual_si =ifelse((Actual<=30),Actual*50/30,
                           ifelse((Actual>30 & Actual<=60),50+(Actual-30)*50/30,
                                  ifelse((Actual>60 & Actual<=90),100+(Actual-60)*100/30,
                                         ifelse((Actual>90 & Actual<=120),200+(Actual-90)*(100/30),
                                                ifelse((Actual>120 & Actual<=250),300+(Actual-120)*(100/130),
                                                       ifelse((Actual>250),400+(Actual-250)*(100/130),NA))))))) %>%
  mutate(rf_si =ifelse((rf_forecast<=30),rf_forecast*50/30,
                        ifelse((rf_forecast>30 & rf_forecast<=60),50+(rf_forecast-30)*50/30,
                               ifelse((rf_forecast>60 & rf_forecast<=90),100+(rf_forecast-60)*100/30,
                                      ifelse((rf_forecast>90 & rf_forecast<=120),200+(rf_forecast-90)*(100/30),
                                             ifelse((rf_forecast>120 & rf_forecast<=250),300+(rf_forecast-120)*(100/130),
                                                    ifelse((rf_forecast>250),400+(rf_forecast-250)*(100/130),NA))))))) %>%
  mutate(actual_aqicat = ifelse((actual_si<=50),"Good",
                                ifelse((actual_si>50 & actual_si<=100),"Satisfactory",
                                       ifelse((actual_si>100 & actual_si<=200),"Moderately Polluted",
                                              ifelse((actual_si>200 & actual_si<=300),"Poor",
                                                     ifelse((actual_si>300 & actual_si<=400),"Very Poor",
                                                            ifelse((actual_si>400),"Severe",NA)))))))  %>%
  mutate(rf_aqicat = ifelse((rf_si<=50),"Good",
                             ifelse((rf_si>50 & rf_si<=100),"Satisfactory",
                                    ifelse((rf_si>100 & rf_si<=200),"Moderately Polluted",
                                           ifelse((rf_si>200 & rf_si<=300),"Poor",
                                                  ifelse((rf_si>300 & rf_si<=400),"Very Poor",
                                                         ifelse((rf_si>400),"Severe",NA)))))))  %>%
  select(Date,actual_aqicat,rf_aqicat) %>%
  rename(Ghaziabad.Observed = actual_aqicat)%>%
  rename(Ghaziabad.Forecasted = rf_aqicat)

write.csv(gha_eval_aqi,"C:/Users/send2/Documents/Ankur/MSc/Temp/dataFile/gha_aqi_cat.csv")

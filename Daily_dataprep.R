library(dplyr)
library(readxl)
library(mice)
library(VIM)
library(ggplot2)
library(corrplot)
library(chron)


## Delhi Punjabi Bagh
## Ghaziabad

###################################################
# Delhi 
###################################################

# Load air quality data
aqdata <- read_excel("Daily.xlsx", sheet="PunjabiBagh",na = "None")
aqdata1 <- aqdata %>%
  mutate(Timeline = as.POSIXct(as.character(aqdata$`From Date`), format="%d-%m-%Y %H:%M")) %>%
  mutate(Date = as.Date(as.character(Timeline))) %>%
  # Remove From Date, To Date and Timeline
  subset(select = -c(`From Date`,`To Date`, Timeline)) %>%
  # Rearrange columns 
  subset(select = c(Date, PM2.5, PM10, Ozone, NO, NO2, NOx, CO, SO2)) %>%
  filter(Date >= '2018-01-01', Date <= '2019-12-31')  
summary(aqdata1)

# Load weather data
dwdata <- read.csv("delpb_wdfd.csv")
dwdata1 <- dwdata %>%
  mutate (Date = as.Date(as.character(dwdata$time))) %>%
  filter(Date >= '2018-01-01', Date <= '2019-12-31') %>% 
  mutate(sunriseTime = as.POSIXct(as.character(sunriseTime), format="%Y-%m-%d %H:%M:%S")) %>%
  mutate(sunsetTime = as.POSIXct(as.character(sunsetTime), format="%Y-%m-%d %H:%M:%S")) %>%
  # Daylength in minutes  
  mutate(Daylen = as.integer(difftime(sunsetTime,sunriseTime,units="mins"))) %>%
  # Removing index and all time variables
  #subset(select = -c(1,2,5,6,12,14,16,18,23,27,30,32,34,36,37))
  subset(select = -c(X, time, sunriseTime, sunsetTime, temperatureHighTime, 
                     temperatureLowTime, apparentTemperatureHighTime, 
                     apparentTemperatureLowTime, uvIndexTime, temperatureMinTime, 
                     temperatureMaxTime, apparentTemperatureMinTime, 
                     apparentTemperatureMaxTime,precipIntensityMaxTime, windGustTime))
summary(dwdata1)

# Join air quality and weather data
aqwdata <- aqdata1 %>%
  left_join(dwdata1, by = c("Date" = "Date"))

## Check data quality
summary(subset(aqwdata, select = c(PM2.5,PM10,Ozone,NO,NO2,NOx,CO,SO2)))
# No records with Pollutant concentration <= 0 
summary(subset(aqwdata, select = c(precipProbability,temperatureHigh,temperatureLow,
                                   apparentTemperatureHigh,apparentTemperatureLow,
                                   dewPoint,humidity,windBearing,temperatureMin,
                                   temperatureMax,apparentTemperatureMin,apparentTemperatureMax)))
# Records with negative temperature
bad_data <- aqwdata %>%
  subset(select = c(Date,precipProbability,temperatureHigh,temperatureLow,
                    apparentTemperatureHigh,apparentTemperatureLow,
                    dewPoint,humidity,windBearing,temperatureMin,
                    temperatureMax,apparentTemperatureMin,apparentTemperatureMax)) %>%
  filter(temperatureMin < 0)


# Invalid data to NA
aqwdata1 <- aqwdata %>%
  mutate(PM2.5 = replace(PM2.5, PM2.5 > 500, 500)) %>%
  mutate(PM10 = replace(PM10, PM10 > 700, 700)) %>%
  mutate(NO2 = replace(NO2, NO2 > 375, 375)) %>%
  mutate(SO2 = replace(SO2, SO2 > 100, 100)) %>%
  mutate(temperatureMin = replace(temperatureMin, temperatureMin < 0, NA)) %>%
  mutate(apparentTemperatureMin = replace(apparentTemperatureMin, apparentTemperatureMin < 0, NA))

### Mising Data Imputation

## Categorical variables
catmis <- subset(aqwdata1, select = c(Date, summary, icon, precipType))
summary(catmis)
# Removing summary and precipType
catclean <- subset(catmis, select = -c(summary, precipType))
# Creating missing factor for icon - "Missing"    
levels(catclean$icon) <- c(levels(catclean$icon), "missing")
catclean$icon = replace(catclean$icon, is.na(catclean$icon), "missing")
summary(catclean)

## Numerical variables
nummis <- subset(aqwdata1, select = -c(Date, summary, icon, precipType)) 
md.pattern(nummis)
mice_plot <- aggr(nummis, col=c('navyblue','yellow'),
                  numbers=TRUE, sortVars=TRUE,
                  labels=names(nummis), cex.axis=.7,
                  gap=3, ylab=c("Missing data","Pattern"))

# Removing following variables with high percentage of missing values
# pressure, ozone, precipIntensity, precipIntensityMax, precipProbability, windGust
# Removing temperatureHigh and apparentTemperatureHigh due to
# high collinearity with temperatureMax and apparentTemperatureMax

#nummisx <- subset(nummis, select = -c(10,11,12,20,29,30,13,15))
nummisx <- subset(nummis, select = -c(pressure, ozone, precipIntensity, precipIntensityMax, 
                                      precipProbability, windGust, temperatureHigh,
                                      apparentTemperatureHigh))
md.pattern(nummisx)
mice_plot <- aggr(nummisx, col=c('navyblue','yellow'),
                  numbers=TRUE, sortVars=TRUE,
                  labels=names(nummisx), cex.axis=.7,
                  gap=3, ylab=c("Missing data","Pattern"))

# Use mice to impute missing data
imputed_data <- mice(nummisx, m=3, maxit = 30, method = 'pmm', seed = 5001, ridge=0.01)
numclean <- mice::complete(imputed_data,2)

# Clean data
aqwclean <- cbind(catclean,numclean) 

### Create temporal features
aqwcleanf <- aqwclean %>%
  mutate(Mnth = month(Date)) %>%
  mutate(MnthName = as.factor(substr(months(Date),1,3))) %>%
  mutate(MnthDay = as.POSIXlt(Date)$mday) %>%
  mutate(Day = as.POSIXlt(Date)$yday + 1) %>%
  mutate(Weekday = as.factor(weekdays(Date))) %>%
  mutate(Weekend = is.weekend(Date))

### AQI features
aqiprep <- aqwcleanf %>%
  subset(select =c("Date","PM2.5","PM10","Ozone","NO2","CO","SO2")) %>%
  mutate(pm25_si =ifelse((PM2.5<=30),PM2.5*50/30,
                         ifelse((PM2.5>30 & PM2.5<=60),50+(PM2.5-30)*50/30,
                                ifelse((PM2.5>60 & PM2.5<=90),100+(PM2.5-60)*100/30,
                                       ifelse((PM2.5>90 & PM2.5<=120),200+(PM2.5-90)*(100/30),
                                              ifelse((PM2.5>120 & PM2.5<=250),300+(PM2.5-120)*(100/130),
                                                     ifelse((PM2.5>250),400+(PM2.5-250)*(100/130),NA))))))) %>%
  mutate(pm10_si = ifelse((PM10<=50),PM10,
                          ifelse((PM10>50 & PM10<=100),PM10,
                                 ifelse((PM10>100 & PM10<=250),100+(PM10-100)*100/150,
                                        ifelse((PM10>250 & PM10<=350),200+(PM10-250),
                                               ifelse((PM10>350 & PM10<=430),300+(PM10-350)*(100/80),
                                                      ifelse((PM10>430),400+(PM10-430)*(100/80),NA))))))) %>%
  mutate(o3_si = ifelse((Ozone<=50),Ozone*50/50,
                        ifelse((Ozone>50 & Ozone<=100),50+(Ozone-50)*50/50,
                               ifelse((Ozone>100 & Ozone<=168),100+(Ozone-100)*100/68,
                                      ifelse((Ozone>168 & Ozone<=208),200+(Ozone-168)*(100/40),
                                             ifelse((Ozone>208 & Ozone<=748),300+(Ozone-208)*(100/539),
                                                    ifelse((Ozone>748),400+(Ozone-400)*(100/539),NA))))))) %>%
  mutate(no2_si = ifelse((NO2<=40),NO2*50/40,
                         ifelse((NO2>40 & NO2<=80),50+(NO2-40)*50/40,
                                ifelse((NO2>80 & NO2<=180),100+(NO2-80)*100/100,
                                       ifelse((NO2>180 & NO2<=280),200+(NO2-180)*(100/100),
                                              ifelse((NO2>280 & NO2<=400),300+(NO2-280)*(100/120),
                                                     ifelse((NO2>400),400+(NO2-400)*(100/120),NA))))))) %>%
  mutate(co_si = ifelse((CO<=1),CO*50/1,
                        ifelse((CO>1 & CO<=2),50+(CO-1)*50/1,
                               ifelse((CO>2 & CO<=10),100+(CO-2)*100/8,
                                      ifelse((CO>10 & CO<=17),200+(CO-10)*(100/7),
                                             ifelse((CO>17 & CO<=34),300+(CO-17)*(100/17),
                                                    ifelse((CO>34),400+(CO-34)*(100/17),NA))))))) %>%
  mutate(so2_si = ifelse((SO2<=40),SO2*50/40,
                         ifelse((SO2>40 & SO2<=80),50+(SO2-40)*50/40,
                                ifelse((SO2>80 & SO2<=380),100+(SO2-80)*100/300,
                                       ifelse((SO2>380 & SO2<=800),200+(SO2-380)*(100/420),
                                              ifelse((SO2>800 & SO2<=1600),300+(SO2-800)*(100/800),
                                                     ifelse((SO2>1600),400+(SO2-1600)*(100/800),NA)))))))

aqifinal <- subset(aqiprep, select =c("Date")) %>%
  mutate(AQI_Value = round(apply(aqiprep[,-c(1:7)], 1, max), digits = 0)) %>%
  mutate(PP = colnames(aqiprep[,-c(1:7)])[max.col(aqiprep[,-c(1:7)],ties.method="first")]) %>%
  mutate(Prominent_Pollutant = ifelse((PP=="pm25_si"),"PM2.5",
                                      ifelse((PP=="pm10_si"),"PM10",
                                             ifelse((PP=="o3_si"),"Ozone",
                                                    ifelse((PP=="no2_si"),"NO2",
                                                           ifelse((PP=="co_si"),"CO",
                                                                  ifelse((PP=="so2_si"),"SO2",NA))))))) %>%
  mutate(AQI_Category = ifelse((AQI_Value<=50),"Good",
                               ifelse((AQI_Value>50 & AQI_Value<=100),"Satisfactory",
                                      ifelse((AQI_Value>100 & AQI_Value<=200),"Moderately Polluted",
                                             ifelse((AQI_Value>200 & AQI_Value<=300),"Poor",
                                                    ifelse((AQI_Value>300 & AQI_Value<=400),"Very Poor",
                                                           ifelse((AQI_Value>400),"Severe",NA))))))) %>%
  # Drop PP column  
  subset(select = -c(3)) %>%
  mutate_at(c("Prominent_Pollutant","AQI_Category"),funs(factor(.)))

# Join aqi and aqw
datafinal <- aqwcleanf %>%
  inner_join(aqifinal, by = c("Date" = "Date")) %>%
  mutate(City = rep("Delhi",nrow(aqwcleanf))) # make new column

# Write RDS
saveRDS(datafinal,"Delhi_Daily.rds")
  
  
# Remove dataset which are not required
rm(aqdata,aqdata1,dwdata,dwdata1,aqwdata,aqwdata1,bad_data,catmis,catclean,nummis,nummisx,
   numclean,imputed_data,mice_plot,aqwclean,aqwcleanf,aqiprep,aqifinal,datafinal)

###################################################
# Ghaziabad
###################################################

# Load air quality data
aqdata <- read_excel("Daily.xlsx", sheet="Ghaziabad",na = "None")
aqdata1 <- aqdata %>%
  mutate(Timeline = as.POSIXct(as.character(aqdata$`From Date`), format="%d-%m-%Y %H:%M")) %>%
  mutate(Date = as.Date(as.character(Timeline))) %>%
  # Remove From Date, To Date and Timeline
  subset(select = -c(`From Date`,`To Date`, Timeline)) %>%
  # Rearrange columns 
  subset(select = c(Date, PM2.5, PM10, Ozone, NO, NO2, NOx, CO, SO2)) %>%
  filter(Date >= '2018-01-01', Date <= '2019-12-31')  
summary(aqdata1)

# Load weather data
dwdata <- read.csv("gha_wdfd.csv")
dwdata1 <- dwdata %>%
  mutate (Date = as.Date(as.character(dwdata$time))) %>%
  filter(Date >= '2018-01-01', Date <= '2019-12-31') %>% 
  mutate(sunriseTime = as.POSIXct(as.character(sunriseTime), format="%Y-%m-%d %H:%M:%S")) %>%
  mutate(sunsetTime = as.POSIXct(as.character(sunsetTime), format="%Y-%m-%d %H:%M:%S")) %>%
  # Daylength in minutes  
  mutate(Daylen = as.integer(difftime(sunsetTime,sunriseTime,units="mins"))) %>%
  # Removing index and all time variables
  #subset(select = -c(1,2,5,6,12,14,16,18,23,27,30,32,34,36,37))
  subset(select = -c(X, time, sunriseTime, sunsetTime, temperatureHighTime, 
                     temperatureLowTime, apparentTemperatureHighTime, 
                     apparentTemperatureLowTime, uvIndexTime, temperatureMinTime, 
                     temperatureMaxTime, apparentTemperatureMinTime, 
                     apparentTemperatureMaxTime,precipIntensityMaxTime, windGustTime))
summary(dwdata1)

# Join air quality and weather data
aqwdata <- aqdata1 %>%
  left_join(dwdata1, by = c("Date" = "Date"))

## Check data quality

summary(subset(aqwdata, select = c(PM2.5,PM10,Ozone,NO,NO2,NOx,CO,SO2)))
# No records with Pollutant concentration <= 0 

summary(subset(aqwdata, select = c(precipProbability,temperatureHigh,temperatureLow,
                                   apparentTemperatureHigh,apparentTemperatureLow,
                                   dewPoint,humidity,windBearing,temperatureMin,
                                   temperatureMax,apparentTemperatureMin,apparentTemperatureMax)))
# Records with negative temperature
bad_data <- aqwdata %>%
  subset(select = c(Date,precipProbability,temperatureHigh,temperatureLow,
                    apparentTemperatureHigh,apparentTemperatureLow,
                    dewPoint,humidity,windBearing,temperatureMin,
                    temperatureMax,apparentTemperatureMin,apparentTemperatureMax)) %>%
  filter(temperatureMin < 0)


# Invalid data to NA
aqwdata1 <- aqwdata %>%
  mutate(PM2.5 = replace(PM2.5, PM2.5 > 500, 500)) %>%
  mutate(PM10 = replace(PM10, PM10 > 700, 700)) %>%
  mutate(NO2 = replace(NO2, NO2 > 375, 375)) %>%
  mutate(SO2 = replace(SO2, SO2 > 100, 100)) %>%
  mutate(temperatureMin = replace(temperatureMin, temperatureMin < 0, NA)) %>%
  mutate(apparentTemperatureMin = replace(apparentTemperatureMin, apparentTemperatureMin < 0, NA))

### Mising Data Imputation

## Categorical variables

catmis <- subset(aqwdata1, select = c(Date, summary, icon, precipType))
summary(catmis)

# Removing summary and precipType
catclean <- subset(catmis, select = -c(summary, precipType))

# Creating missing factor for icon - "missing"    
levels(catclean$icon) <- c(levels(catclean$icon), "missing")
catclean$icon = replace(catclean$icon, is.na(catclean$icon), "missing")
summary(catclean)

## Numerical variables

nummis <- subset(aqwdata1, select = -c(Date, summary, icon, precipType)) 
md.pattern(nummis)
mice_plot <- aggr(nummis, col=c('navyblue','yellow'),
                  numbers=TRUE, sortVars=TRUE,
                  labels=names(nummis), cex.axis=.7,
                  gap=3, ylab=c("Missing data","Pattern"))

# Removing following variables with high percentage of missing values
# pressure, ozone
# Remove following variable to keep consistency with other cities
# precipIntensity, precipIntensityMax, precipProbability, windGust
# Removing temperatureHigh and apparentTemperatureHigh due to
# high collinearity with temperatureMax and apparentTemperatureMax

#nummisx <- subset(nummis, select = -c(10,11,12,20,29,30,13,15))
nummisx <- subset(nummis, select = -c(pressure, ozone, precipIntensity, precipIntensityMax, 
                                      precipProbability, windGust, temperatureHigh,
                                      apparentTemperatureHigh))
md.pattern(nummisx)
mice_plot <- aggr(nummisx, col=c('navyblue','yellow'),
                  numbers=TRUE, sortVars=TRUE,
                  labels=names(nummisx), cex.axis=.7,
                  gap=3, ylab=c("Missing data","Pattern"))

# Use mice to impute missing data
imputed_data <- mice(nummisx, m=3, maxit = 30, method = 'pmm', seed = 5001, ridge=0.01)
numclean <- mice::complete(imputed_data,2)

# Clean data
aqwclean <- cbind(catclean,numclean) 

### Create temporal features
aqwcleanf <- aqwclean %>%
  mutate(Mnth = month(Date)) %>%
  mutate(MnthName = as.factor(substr(months(Date),1,3))) %>%
  mutate(MnthDay = as.POSIXlt(Date)$mday) %>%
  mutate(Day = as.POSIXlt(Date)$yday + 1) %>%
  mutate(Weekday = as.factor(weekdays(Date))) %>%
  mutate(Weekend = is.weekend(Date))

### AQI features
aqiprep <- aqwcleanf %>%
  subset(select =c("Date","PM2.5","PM10","Ozone","NO2","CO","SO2")) %>%
  mutate(pm25_si =ifelse((PM2.5<=30),PM2.5*50/30,
                         ifelse((PM2.5>30 & PM2.5<=60),50+(PM2.5-30)*50/30,
                                ifelse((PM2.5>60 & PM2.5<=90),100+(PM2.5-60)*100/30,
                                       ifelse((PM2.5>90 & PM2.5<=120),200+(PM2.5-90)*(100/30),
                                              ifelse((PM2.5>120 & PM2.5<=250),300+(PM2.5-120)*(100/130),
                                                     ifelse((PM2.5>250),400+(PM2.5-250)*(100/130),NA))))))) %>%
  mutate(pm10_si = ifelse((PM10<=50),PM10,
                          ifelse((PM10>50 & PM10<=100),PM10,
                                 ifelse((PM10>100 & PM10<=250),100+(PM10-100)*100/150,
                                        ifelse((PM10>250 & PM10<=350),200+(PM10-250),
                                               ifelse((PM10>350 & PM10<=430),300+(PM10-350)*(100/80),
                                                      ifelse((PM10>430),400+(PM10-430)*(100/80),NA))))))) %>%
  mutate(o3_si = ifelse((Ozone<=50),Ozone*50/50,
                        ifelse((Ozone>50 & Ozone<=100),50+(Ozone-50)*50/50,
                               ifelse((Ozone>100 & Ozone<=168),100+(Ozone-100)*100/68,
                                      ifelse((Ozone>168 & Ozone<=208),200+(Ozone-168)*(100/40),
                                             ifelse((Ozone>208 & Ozone<=748),300+(Ozone-208)*(100/539),
                                                    ifelse((Ozone>748),400+(Ozone-400)*(100/539),NA))))))) %>%
  mutate(no2_si = ifelse((NO2<=40),NO2*50/40,
                         ifelse((NO2>40 & NO2<=80),50+(NO2-40)*50/40,
                                ifelse((NO2>80 & NO2<=180),100+(NO2-80)*100/100,
                                       ifelse((NO2>180 & NO2<=280),200+(NO2-180)*(100/100),
                                              ifelse((NO2>280 & NO2<=400),300+(NO2-280)*(100/120),
                                                     ifelse((NO2>400),400+(NO2-400)*(100/120),NA))))))) %>%
  mutate(co_si = ifelse((CO<=1),CO*50/1,
                        ifelse((CO>1 & CO<=2),50+(CO-1)*50/1,
                               ifelse((CO>2 & CO<=10),100+(CO-2)*100/8,
                                      ifelse((CO>10 & CO<=17),200+(CO-10)*(100/7),
                                             ifelse((CO>17 & CO<=34),300+(CO-17)*(100/17),
                                                    ifelse((CO>34),400+(CO-34)*(100/17),NA))))))) %>%
  mutate(so2_si = ifelse((SO2<=40),SO2*50/40,
                         ifelse((SO2>40 & SO2<=80),50+(SO2-40)*50/40,
                                ifelse((SO2>80 & SO2<=380),100+(SO2-80)*100/300,
                                       ifelse((SO2>380 & SO2<=800),200+(SO2-380)*(100/420),
                                              ifelse((SO2>800 & SO2<=1600),300+(SO2-800)*(100/800),
                                                     ifelse((SO2>1600),400+(SO2-1600)*(100/800),NA)))))))

aqifinal <- subset(aqiprep, select =c("Date")) %>%
  mutate(AQI_Value = round(apply(aqiprep[,-c(1:7)], 1, max), digits = 0)) %>%
  mutate(PP = colnames(aqiprep[,-c(1:7)])[max.col(aqiprep[,-c(1:7)],ties.method="first")]) %>%
  mutate(Prominent_Pollutant = ifelse((PP=="pm25_si"),"PM2.5",
                                      ifelse((PP=="pm10_si"),"PM10",
                                             ifelse((PP=="o3_si"),"Ozone",
                                                    ifelse((PP=="no2_si"),"NO2",
                                                           ifelse((PP=="co_si"),"CO",
                                                                  ifelse((PP=="so2_si"),"SO2",NA))))))) %>%
  mutate(AQI_Category = ifelse((AQI_Value<=50),"Good",
                               ifelse((AQI_Value>50 & AQI_Value<=100),"Satisfactory",
                                      ifelse((AQI_Value>100 & AQI_Value<=200),"Moderately Polluted",
                                             ifelse((AQI_Value>200 & AQI_Value<=300),"Poor",
                                                    ifelse((AQI_Value>300 & AQI_Value<=400),"Very Poor",
                                                           ifelse((AQI_Value>400),"Severe",NA))))))) %>%
  # Drop PP column  
  subset(select = -c(3)) %>%
  mutate_at(c("Prominent_Pollutant","AQI_Category"),funs(factor(.)))

# Join aqw and aqi
datafinal <- aqwcleanf %>%
  inner_join(aqifinal, by = c("Date" = "Date")) %>%
  mutate(City = rep("Ghaziabad",nrow(aqwcleanf))) # make new column

# Write RDS
saveRDS(datafinal,"Ghaziabad_Daily.rds")


# Remove dataset which are not required
rm(aqdata,aqdata1,dwdata,dwdata1,aqwdata,aqwdata1,bad_data,catmis,catclean,nummis,nummisx,
   numclean,imputed_data,mice_plot,aqwclean,aqwcleanf,aqiprep,aqifinal,datafinal)


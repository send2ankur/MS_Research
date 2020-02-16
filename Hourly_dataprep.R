library(dplyr)
library(readxl)
library(mice)
library(VIM)
library(ggplot2)
library(corrplot)
library(chron)
library(data.table)

#####################################################
# Delhi Punjabi Bagh
#####################################################

# Load air quality data
aqdata <- read_excel("Hourly.xlsx", sheet="PunjabiBagh",na = "None")
aqdata1 <- aqdata %>%
  mutate(Timeline = as.POSIXct(as.character(aqdata$`From Date`), format="%d-%m-%Y %H:%M")) %>%
  mutate(Date = as.Date(as.character(Timeline))) %>%
  # Remove From Date, To Date and Timeline
  subset(select = -c(`From Date`,`To Date`)) %>%
  # Rearrange columns 
  subset(select = c(Date, Timeline, PM2.5, PM10, Ozone, NO, NO2, NOx, CO, SO2)) %>%
  filter(Date >= '2018-01-01', Date <= '2020-01-31')  
summary(aqdata1)

# Load weather data
dwdata <- bind_rows(read.csv("delpb_wdfh.csv"),read.csv("delpb_wdfh_2020.csv"))
dwdata1 <- dwdata %>%
  mutate(time = as.POSIXct(as.character(time), format="%Y-%m-%d %H:%M:%S")) %>%
  filter(time >= '2018-01-01 00:00:00', time <= '2020-01-31 23:00:00') %>% 
  subset(select = -c(X))
summary(dwdata1)

# Join air quality and weather data
aqwdata <- aqdata1 %>%
  left_join(dwdata1, by = c("Timeline" = "time"))
summary(aqwdata)

## Check data quality

summary(subset(aqwdata, select = c(PM2.5,PM10,Ozone,NO,NO2,NOx,CO,SO2)))
# Records with Pollutant concentration CO and NOx = 0
bad_data <- aqwdata %>%
  subset(select = c(Timeline,PM2.5,PM10,Ozone,NO,NO2,NOx,CO,SO2)) %>%
  filter(CO == 0 | NOx == 0)

summary(subset(aqwdata, select = c(temperature,apparentTemperature,humidity,windBearing)))
# Records with negative temperature
bad_data <- aqwdata %>%
  subset(select = c(Timeline,dewPoint,temperature,apparentTemperature,humidity,windBearing)) %>%
  filter( temperature < 0 | apparentTemperature < 0 | (temperature < 0 & dewPoint < 0))
  
ggplot(data=aqwdata, aes(y=PM2.5)) + geom_boxplot()
ggplot(data=aqwdata, aes(y=PM10)) + geom_boxplot()
ggplot(data=aqwdata, aes(y=Ozone)) + geom_boxplot()
ggplot(data=aqwdata, aes(y=NO)) + geom_boxplot()
ggplot(data=aqwdata, aes(y=NO2)) + geom_boxplot()
ggplot(data=aqwdata, aes(y=NOx)) + geom_boxplot()
ggplot(data=aqwdata, aes(y=CO)) + geom_boxplot()
ggplot(data=aqwdata, aes(y=SO2)) + geom_boxplot()


ggplot(data=aqwdata, aes(x=Timeline, y=PM2.5)) + geom_line(color="red") + geom_point()
ggplot(data=aqwdata, aes(x=Timeline, y=PM10)) + geom_line(color="red") + geom_point()


# Invalid data to NA
aqwdata1 <- aqwdata %>%
  mutate(mnth = month(Timeline)) %>%
  mutate(PM2.5 = replace(PM2.5, PM2.5 > 500, NA)) %>%
  #mutate(PM2.5 = replace(PM2.5, (PM2.5 > 500 & mnth %in% c(3,4,5,6,7,8,9,10)), NA)) %>%
  mutate(PM10 = replace(PM10, PM10 > 700, NA)) %>%
  #mutate(PM10 = replace(PM10, (PM10 > 650 & mnth %in% c(3,4,5,6,7,8,9,10)), NA)) %>%
  mutate(NO2 = replace(NO2, NO2 > 375, NA)) %>%
  mutate(NOx = replace(NOx, NOx == 0, NA)) %>%
  mutate(CO = replace(CO, CO == 0, NA)) %>%
  mutate(SO2 = replace(SO2, SO2 > 100, NA)) %>%
  mutate(dewPoint = replace(dewPoint, temperature < 0, NA)) %>%
  mutate(temperature = replace(temperature, temperature < 0, NA )) %>%
  mutate(apparentTemperature = replace(apparentTemperature, apparentTemperature < 0, NA ))%>%
  subset(select = -c(mnth))

ggplot(data=aqwdata1, aes(x=Timeline, y=PM2.5)) + geom_line(color="red") + geom_point()
ggplot(data=aqwdata1, aes(x=Timeline, y=PM10)) + geom_line(color="red") + geom_point()


### Mising Data Imputation

## Categorical variables

catmis <- subset(aqwdata1, select = c(Date, Timeline, summary, icon, precipType))
catmis$summary <- as.factor(catmis$summary)
catmis$icon <- as.factor(catmis$icon)
catmis$precipType <- as.factor(catmis$precipType)
summary(catmis)

# Removing summary and precipType, renaming icon to summary
catclean <- subset(catmis, select = -c(summary, precipType)) %>%
  rename(summary = icon)

# Creating missing factor for summary - "missing"    
levels(catclean$summary) <- c(levels(catclean$summary), "missing")
catclean$summary = replace(catclean$summary, is.na(catclean$summary), "missing")
summary(catclean)

## Numerical variables

nummis <- subset(aqwdata1, select = -c(Date, Timeline, summary, icon, precipType)) 
md.pattern(nummis)
mice_plot <- aggr(nummis, col=c('navyblue','yellow'),
                  numbers=TRUE, sortVars=TRUE,
                  labels=names(nummis), cex.axis=.7,
                  gap=3, ylab=c("Missing data","Pattern"))

# Removing following variables with high percentage of missing values
# ozone
nummisx <- subset(nummis, select = -c(ozone))
md.pattern(nummisx)
mice_plot <- aggr(nummisx, col=c('navyblue','yellow'),
                  numbers=TRUE, sortVars=TRUE,
                  labels=names(nummisx), cex.axis=.7,
                  gap=3, ylab=c("Missing data","Pattern"))

# Use mice to impute missing data
imputed_data <- mice(nummisx, m=3, maxit = 30, method = 'pmm', seed = 5001, ridge=0.01)
numclean <- mice::complete(imputed_data,2)
saveRDS(imputed_data,"df_delpb_hourly_mice.rds")

# Clean data
aqwclean <- cbind(catclean,numclean) 

## Create Temporal variables
aqwcleanf <- aqwclean %>%
  mutate(Mnth = month(Timeline)) %>%
  mutate(MnthName = as.factor(substr(months(Date),1,3))) %>%
  mutate(MnthDay = as.POSIXlt(Date)$mday) %>%
  mutate(Day = as.POSIXlt(Date)$yday + 1) %>%
  mutate(Weekday = as.factor(weekdays(Date))) %>%
  mutate(Weekend = is.weekend(as.Date(as.character(Timeline)))) %>%
  mutate(Hour = hour(Timeline)) %>%
  mutate(City = rep("Delhi",nrow(aqwclean))) %>% 
  mutate(TimeofDay = as.factor(cut(as.integer(Hour), c(-1,3,7,11,15,19,23),
                                   labels=c('MidNight','EarlyMorning','Morning'
                                            ,'DayTime','Evening','Night')))) %>%
  mutate(NewYear = ifelse((Date == '2018-12-31'),1,
                               ifelse((Date == '2019-01-01'),1,
                                      ifelse((Date == '2019-12-31'),1,
                                             ifelse((Date == '2020-01-01'),1,0)))))

saveRDS(aqwcleanf,"df_delpb_hourly.rds")


# Remove dataset which are not required
rm(aqdata,aqdata1,dwdata,dwdata1,aqwdata,aqwdata1,bad_data,catmis,catclean,nummis,nummisx,
   numclean,imputed_data,mice_plot,aqwclean,aqwcleanf)


#####################################################
# Ghaziabad
#####################################################

# Load air quality data
aqdata <- read_excel("Hourly.xlsx", sheet="Ghaziabad",na = "None")
aqdata1 <- aqdata %>%
  mutate(Timeline = as.POSIXct(as.character(aqdata$`From Date`), format="%d-%m-%Y %H:%M")) %>%
  mutate(Date = as.Date(as.character(Timeline))) %>%
  # Remove From Date, To Date and Timeline
  subset(select = -c(`From Date`,`To Date`)) %>%
  # Rearrange columns 
  subset(select = c(Date, Timeline, PM2.5, PM10, Ozone, NO, NO2, NOx, CO, SO2)) %>%
  filter(Date >= '2018-01-01', Date <= '2020-01-31')  
summary(aqdata1)

# Load weather data
dwdata <- bind_rows(read.csv("gha_wdfh.csv"),read.csv("gha_wdfh_2020.csv"))
dwdata1 <- dwdata %>%
  mutate(time = as.POSIXct(as.character(time), format="%Y-%m-%d %H:%M:%S")) %>%
  filter(time >= '2018-01-01 00:00:00', time <= '2020-01-31 23:00:00') %>% 
  subset(select = -c(X))
summary(dwdata1)

# Join air quality and weather data
aqwdata <- aqdata1 %>%
  left_join(dwdata1, by = c("Timeline" = "time"))
summary(aqwdata)

## Check data quality

summary(subset(aqwdata, select = c(PM2.5,PM10,Ozone,NO,NO2,NOx,CO,SO2)))
# Records with Pollutant concentration CO and NOx = 0
bad_data <- aqwdata %>%
  subset(select = c(Timeline,PM2.5,PM10,Ozone,NO,NO2,NOx,CO,SO2)) %>%
  filter(CO == 0 | NOx == 0)

summary(subset(aqwdata, select = c(temperature,apparentTemperature,humidity,windBearing)))
# Records with negative temperature
bad_data <- aqwdata %>%
  subset(select = c(Timeline,dewPoint,temperature,apparentTemperature,humidity,windBearing)) %>%
  filter( temperature < 0 | apparentTemperature < 0 | (temperature < 0 & dewPoint < 0))


ggplot(data=aqwdata, aes(y=PM2.5)) + geom_boxplot()
ggplot(data=aqwdata, aes(y=PM10)) + geom_boxplot()
ggplot(data=aqwdata, aes(y=Ozone)) + geom_boxplot()
ggplot(data=aqwdata, aes(y=NO)) + geom_boxplot()
ggplot(data=aqwdata, aes(y=NO2)) + geom_boxplot()
ggplot(data=aqwdata, aes(y=NOx)) + geom_boxplot()
ggplot(data=aqwdata, aes(y=CO)) + geom_boxplot()
ggplot(data=aqwdata, aes(y=SO2)) + geom_boxplot()


ggplot(data=aqwdata, aes(x=Timeline, y=PM2.5)) + geom_line(color="red") + geom_point()
ggplot(data=aqwdata, aes(x=Timeline, y=PM10)) + geom_line(color="red") + geom_point()


# Invalid data to NA
aqwdata1 <- aqwdata %>%
  mutate(mnth = month(Timeline)) %>%
  mutate(PM2.5 = replace(PM2.5, PM2.5 > 500, NA)) %>%
  #mutate(PM2.5 = replace(PM2.5, (PM2.5 > 500 & mnth %in% c(3,4,5,6,7,8,9,10)), NA)) %>%
  mutate(PM10 = replace(PM10, PM10 > 700, NA)) %>%
  #mutate(PM10 = replace(PM10, (PM10 > 650 & mnth %in% c(3,4,5,6,7,8,9,10)), NA)) %>%
  mutate(NO2 = replace(NO2, NO2 > 375, NA)) %>%
  mutate(NOx = replace(NOx, NOx == 0, NA)) %>%
  mutate(CO = replace(CO, CO == 0, NA)) %>%
  mutate(SO2 = replace(SO2, SO2 > 100, NA)) %>%
  mutate(dewPoint = replace(dewPoint, temperature < 0, NA)) %>%
  mutate(temperature = replace(temperature, temperature < 0, NA )) %>%
  mutate(apparentTemperature = replace(apparentTemperature, apparentTemperature < 0, NA ))%>%
  subset(select = -c(mnth))

ggplot(data=aqwdata1, aes(x=Timeline, y=PM2.5)) + geom_line(color="red") + geom_point()
ggplot(data=aqwdata1, aes(x=Timeline, y=PM10)) + geom_line(color="red") + geom_point()

### Mising Data Imputation

## Categorical variables

catmis <- subset(aqwdata1, select = c(Date, Timeline, summary, icon, precipType))
catmis$summary <- as.factor(catmis$summary)
catmis$icon <- as.factor(catmis$icon)
catmis$precipType <- as.factor(catmis$precipType)
summary(catmis)

# Removing summary and precipType, renaming icon to summary
catclean <- subset(catmis, select = -c(summary, precipType)) %>%
  rename(summary = icon)

# Creating missing factor for summary - "missing"    
levels(catclean$summary) <- c(levels(catclean$summary), "missing")
catclean$summary = replace(catclean$summary, is.na(catclean$summary), "missing")
summary(catclean)

## Numerical variables

nummis <- subset(aqwdata1, select = -c(Date, Timeline, summary, icon, precipType)) 
md.pattern(nummis)
mice_plot <- aggr(nummis, col=c('navyblue','yellow'),
                  numbers=TRUE, sortVars=TRUE,
                  labels=names(nummis), cex.axis=.7,
                  gap=3, ylab=c("Missing data","Pattern"))

# Removing following variables with high percentage of missing values
# ozone
nummisx <- subset(nummis, select = -c(ozone))
md.pattern(nummisx)
mice_plot <- aggr(nummisx, col=c('navyblue','yellow'),
                  numbers=TRUE, sortVars=TRUE,
                  labels=names(nummisx), cex.axis=.7,
                  gap=3, ylab=c("Missing data","Pattern"))

# Use mice to impute missing data
imputed_data <- mice(nummisx, m=3, maxit = 30, method = 'pmm', seed = 5001, ridge=0.01)
numclean <- mice::complete(imputed_data,2)
saveRDS(imputed_data,"df_gha_hourly_mice.rds")

# Clean data
aqwclean <- cbind(catclean,numclean) 

## Create Temporal variables
aqwcleanf <- aqwclean %>%
  mutate(Mnth = month(Timeline)) %>%
  mutate(MnthName = as.factor(substr(months(Date),1,3))) %>%
  mutate(MnthDay = as.POSIXlt(Date)$mday) %>%
  mutate(Day = as.POSIXlt(Date)$yday + 1) %>%
  mutate(Weekday = as.factor(weekdays(Date))) %>%
  mutate(Weekend = is.weekend(as.Date(as.character(Timeline)))) %>%
  mutate(Hour = hour(Timeline)) %>%
  mutate(City = rep("Ghaziabad",nrow(aqwclean))) %>% 
  mutate(TimeofDay = as.factor(cut(as.integer(Hour), c(-1,3,7,11,15,19,23),
                                   labels=c('MidNight','EarlyMorning',
                                            'Morning','DayTime','Evening','Night')))) %>%
  mutate(NewYear = ifelse((Date == '2018-12-31'),1,
                          ifelse((Date == '2019-01-01'),1,
                                 ifelse((Date == '2019-12-31'),1,
                                        ifelse((Date == '2020-01-01'),1,0)))))


saveRDS(aqwcleanf,"df_gha_hourly.rds")

# Remove dataset which are not required
rm(aqdata,aqdata1,dwdata,dwdata1,aqwdata,aqwdata1,bad_data,catmis,catclean,nummis,nummisx,
   numclean,imputed_data,mice_plot,aqwclean,aqwcleanf)


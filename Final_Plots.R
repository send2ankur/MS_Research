library(dplyr)
library(viridis)
library(RColorBrewer)
library(corrplot)
library(grid)
library(gridExtra)
library(openair)
library(cowplot)
library(ggplot2)
## ggplot theme setting
theme_set(theme_bw())


##############################################################################
##############################################################################
##############################################################################
###
###      Daily Plots
###
##############################################################################
##############################################################################
##############################################################################

df <- bind_rows(readRDS("Delhi_Daily.rds"),
                readRDS("Ghaziabad_Daily.rds"))

aqdf <- df %>%
  mutate(yearM = paste(substring(Date,1,7))) %>%
  mutate(year = paste(substring(Date,1,4))) %>%
  mutate(PM2.5 = replace(PM2.5, PM2.5 > 500, 500)) %>%
  mutate(PM10 = replace(PM10, PM10 > 700, 700)) %>%
  mutate(NO2 = replace(NO2, NO2 > 375, 375)) %>%
  mutate(SO2 = replace(SO2, SO2 > 100, 100)) %>%
  subset(select = c(Date,PM2.5,PM10,Ozone,NO,NO2,NOx,CO, SO2,
                    Mnth,MnthName,MnthDay,Day,Weekday,Weekend,year,yearM,
                    Prominent_Pollutant,AQI_Value,AQI_Category,City))

##############################################################################
# Delhi 

# PM2.5 Scatter Plot
Plot1 <- aqdf %>%
  filter(City == "Delhi" & PM2.5 < 500) %>%
  ggplot(aes(x=Date, y=PM2.5)) + 
  geom_point()+
  geom_smooth() +
  scale_x_date(date_breaks = "6 months") + 
  labs(x = "Date", y = expression(PM[2.5]~concentration)) 

# PM10 Scatter Plot
Plot2 <- aqdf %>%
  filter(City == "Delhi" & PM10 < 750) %>%
  ggplot(aes(x=Date, y=PM10)) + 
  geom_point()+
  geom_smooth() +
  scale_x_date(date_breaks = "6 months") + 
  labs(x = "Date", y = expression(PM[10]~concentration))

# Ozone Scatter Plot
Plot3 <- aqdf %>%
  filter(City == "Delhi" & Ozone < 150) %>%
  ggplot(aes(x=Date, y=Ozone)) + 
  geom_point()+
  geom_smooth() +
  scale_x_date(date_breaks = "6 months") + 
  labs(x = "Date", y = expression(Ozone~concentration))

# CO Scatter Plot
Plot4 <- aqdf %>%
  filter(City == "Delhi" ) %>%
  ggplot(aes(x=Date, y=CO)) + 
  geom_point()+
  geom_smooth() +
  scale_x_date(date_breaks = "6 months") + 
  labs(x = "Date", y = expression(CO~concentration))

grid.arrange(Plot1,Plot2,Plot3,Plot4,
             nrow = 2, ncol =2,
             top = "Pollutant concentration in Delhi")

# NO Scatter Plot
Plot1 <- aqdf %>%
  filter(City == "Delhi" & NO < 200) %>%
  ggplot(aes(x=Date, y=NO)) + 
  geom_point()+
  geom_smooth() +
  scale_x_date(date_breaks = "6 months") + 
  labs(x = "Date", y = expression(NO~concentration)) 

# NO2 Scatter Plot
Plot2 <- aqdf %>%
  filter(City == "Delhi" ) %>%
  ggplot(aes(x=Date, y=NO2)) + 
  geom_point()+
  geom_smooth() +
  scale_x_date(date_breaks = "6 months") + 
  labs(x = "Date", y = expression(NO[2]~concentration))

# NOx Scatter Plot
Plot3 <- aqdf %>%
  filter(City == "Delhi" & NOx < 300) %>%
  ggplot(aes(x=Date, y=NOx)) + 
  geom_point()+
  geom_smooth() +
  scale_x_date(date_breaks = "6 months") + 
  labs(x = "Date", y = expression(NO[x]~concentration))

# SO2 Scatter Plot
Plot4 <- aqdf %>%
  filter(City == "Delhi" ) %>%
  ggplot(aes(x=Date, y=SO2)) + 
  geom_point()+
  geom_smooth() +
  scale_x_date(date_breaks = "6 months") + 
  labs(x = "Date", y = expression(SO[2]~concentration))

grid.arrange(Plot1,Plot2,Plot3,Plot4,
             nrow = 2, ncol =2,
             top = "Pollutant concentration in Delhi")

##############################################################################
# Ghaziabad 

# PM2.5 Scatter Plot
Plot1 <- aqdf %>%
  filter(City == "Ghaziabad" & PM2.5 < 500) %>%
  ggplot(aes(x=Date, y=PM2.5)) + 
  geom_point()+
  geom_smooth() +
  scale_x_date(date_breaks = "6 months") + 
  labs(x = "Date", y = expression(PM[2.5]~concentration)) 

# PM10 Scatter Plot
Plot2 <- aqdf %>%
  filter(City == "Ghaziabad" & PM10 < 750) %>%
  ggplot(aes(x=Date, y=PM10)) + 
  geom_point()+
  geom_smooth() +
  scale_x_date(date_breaks = "6 months") + 
  labs(x = "Date", y = expression(PM[10]~concentration))

# Ozone Scatter Plot
Plot3 <- aqdf %>%
  filter(City == "Ghaziabad" & Ozone < 150) %>%
  ggplot(aes(x=Date, y=Ozone)) + 
  geom_point()+
  geom_smooth() +
  scale_x_date(date_breaks = "6 months") + 
  labs(x = "Date", y = expression(Ozone~concentration))

# CO Scatter Plot
Plot4 <- aqdf %>%
  filter(City == "Ghaziabad" ) %>%
  ggplot(aes(x=Date, y=CO)) + 
  geom_point()+
  geom_smooth() +
  scale_x_date(date_breaks = "6 months") + 
  labs(x = "Date", y = expression(CO~concentration))

grid.arrange(Plot1,Plot2,Plot3,Plot4,
             nrow = 2, ncol =2,
             top = "Pollutant concentration in Ghaziabad")

# NO Scatter Plot
Plot1 <- aqdf %>%
  filter(City == "Ghaziabad" & NO < 200) %>%
  ggplot(aes(x=Date, y=NO)) + 
  geom_point()+
  geom_smooth() +
  scale_x_date(date_breaks = "6 months") + 
  labs(x = "Date", y = expression(NO~concentration)) 

# NO2 Scatter Plot
Plot2 <- aqdf %>%
  filter(City == "Ghaziabad" ) %>%
  ggplot(aes(x=Date, y=NO2)) + 
  geom_point()+
  geom_smooth() +
  scale_x_date(date_breaks = "6 months") + 
  labs(x = "Date", y = expression(NO[2]~concentration))

# NOx Scatter Plot
Plot3 <- aqdf %>%
  filter(City == "Ghaziabad" & NOx < 300) %>%
  ggplot(aes(x=Date, y=NOx)) + 
  geom_point()+
  geom_smooth() +
  scale_x_date(date_breaks = "6 months") + 
  labs(x = "Date", y = expression(NO[x]~concentration))

# SO2 Scatter Plot
Plot4 <- aqdf %>%
  filter(City == "Ghaziabad" ) %>%
  ggplot(aes(x=Date, y=SO2)) + 
  geom_point()+
  geom_smooth() +
  scale_x_date(date_breaks = "6 months") + 
  labs(x = "Date", y = expression(SO[2]~concentration))

grid.arrange(Plot1,Plot2,Plot3,Plot4,
             nrow = 2, ncol =2,
             top = "Pollutant concentration in Ghaziabad")

##############################################################################




##############################################################################3
# AQI Category Grouped By City - stacked bar


aqdf %>%
  select(AQI_Category,City,year) %>%
  mutate(AQI_Category=factor(AQI_Category, levels=c("Good","Satisfactory",
                                                    "Moderately Polluted",
                                                    "Poor","Very Poor","Severe"))) %>%
  group_by(AQI_Category,City,year) %>%
  summarise(count=n()) %>%
  ggplot(aes(fill=AQI_Category, y=count, x=City)) + 
  geom_bar(position="fill", stat="identity") +
  #scale_fill_discrete(breaks=aqi_order) + 
  facet_grid(cols=vars(year)) +
  scale_fill_manual(values = c("chartreuse4","darkolivegreen3","rosybrown2",
                               "darkgoldenrod1","brown1","brown3"),
                    name="AQI Category") +
  labs(title="", 
       subtitle="",
       x="City",
       y="Percentage of Days in a year")

##############################################################################3
# AQI Category Grouped By City Weekday Weekend - stacked bar


aqdf %>%
  select(AQI_Category,City,Weekend) %>%
  mutate(Daytype = factor(Weekend, levels = c(TRUE,FALSE), 
                          labels = c("Weekend", "Weekday"))) %>%
  mutate(AQI_Category=factor(AQI_Category, levels=c("Good","Satisfactory",
                                                    "Moderately Polluted",
                                                    "Poor","Very Poor","Severe"))) %>%
  group_by(AQI_Category,City,Daytype) %>%
  summarise(count=n()) %>%
  ggplot(aes(fill=AQI_Category, y=count, x=City)) + 
  geom_bar(position="fill", stat="identity") +
  #scale_fill_discrete(breaks=aqi_order) + 
  facet_grid(cols=vars(Daytype)) +
  scale_fill_manual(values = c("chartreuse4","darkolivegreen3","rosybrown2",
                               "darkgoldenrod1","brown1","brown3"),
                    name="AQI Category") +
  labs(title="", 
       subtitle="",
       x="City",
       y="Percentage of Days in Weekday/Weekend")

##############################################################################
# AQI Category Grouped By City Weather - stacked bar

aqdf %>%
  mutate(WeatherSummary = df$icon) %>%
  filter(WeatherSummary != "missing") %>%
  select(AQI_Category,City,WeatherSummary) %>%
  #mutate(Daytype = factor(Weekend, levels = c(TRUE,FALSE), 
  #                        labels = c("Weekend", "Weekday"))) %>%
  mutate(AQI_Category=factor(AQI_Category, levels=c("Good","Satisfactory",
                                                    "Moderately Polluted",
                                                    "Poor","Very Poor","Severe"))) %>%
  group_by(AQI_Category,City,WeatherSummary) %>%
  summarise(count=n()) %>%
  ggplot(aes(fill=AQI_Category, y=count, x=WeatherSummary)) + 
  geom_bar(position="fill", stat="identity") +
  #scale_fill_discrete(breaks=aqi_order) + 
  facet_grid(cols=vars(City)) +
  scale_fill_manual(values = c("chartreuse4","darkolivegreen3","rosybrown2",
                               "darkgoldenrod1","brown1","brown3"),
                    name="AQI Category") +
  labs(title="", 
       subtitle="",
       x="Weather Summary",
       y="Percentage of Days")

##############################################################################
# Prominent Pollutant by City - Pollutation days 

aqdf %>%
  filter(AQI_Category %in% c("Moderately Polluted","Poor","Very Poor","Severe")) %>%
  select(Prominent_Pollutant,City) %>%
  group_by(Prominent_Pollutant,City) %>%
  summarise(count=n()) %>%
  ggplot(aes(x=Prominent_Pollutant, y=count,fill=Prominent_Pollutant)) + 
  geom_bar(stat="identity") +
  facet_grid(cols=vars(City)) +
  labs(title="Prominent Pollutant", 
       subtitle="",
       x="Prominent Pollutant",
       y="Number of Days")
	   
	   
##############################################################################
##############################################################################
##############################################################################
###
###      Hourly Plots
###
##############################################################################
##############################################################################
##############################################################################


rm(aqdf,df)

df <- bind_rows(readRDS("df_delpb_hourly.rds"),
                readRDS("df_gha_hourly.rds"))

##########################################################################################
# Effect of Weather Summary - PM
##########################################################################################

### Box Plot PM2.5
df %>%
  filter(summary != "missing") %>%
  ggplot(aes(y=PM2.5,fill=summary)) + 
  geom_boxplot() +
  scale_color_brewer(palette = "Dark2", name="Weather Summary") +
  scale_fill_discrete(name="Weather Summary") +
  #theme(legend.position="none") + 
  labs(x = "", y = expression(PM[2.5]~concentration))

### Box Plot PM10
df %>%
  filter(summary != "missing") %>%
  ggplot(aes(y=PM10,fill=summary)) + 
  geom_boxplot() +
  scale_color_brewer(palette = "Dark2", name="Weather Summary") +
  scale_fill_discrete(name="Weather Summary") +
  #theme(legend.position="none") + 
  labs(x = "", y = expression(PM[10]~concentration))

### Box Plot Ozone
df %>%
  filter(summary != "missing") %>%
  ggplot(aes(y=Ozone,fill=summary)) + 
  geom_boxplot() +
  scale_color_brewer(palette = "Dark2", name="Weather Summary") +
  scale_fill_discrete(name="Weather Summary") +
  #theme(legend.position="none") + 
  labs(x = "", y = expression(Ozone~concentration))

### Box Plot NO2
df %>%
  filter(summary != "missing") %>%
  ggplot(aes(y=NO2,fill=summary)) + 
  geom_boxplot() +
  scale_color_brewer(palette = "Dark2", name="Weather Summary") +
  scale_fill_discrete(name="Weather Summary") +
  #theme(legend.position="none") + 
  labs(x = "", y = expression(NO[2]~concentration))

### Box Plot CO
df %>%
  filter(summary != "missing") %>%
  ggplot(aes(y=CO,fill=summary)) + 
  geom_boxplot() +
  scale_color_brewer(palette = "Dark2", name="Weather Summary") +
  scale_fill_discrete(name="Weather Summary") +
  #theme(legend.position="none") + 
  labs(x = "", y = expression(CO~concentration))

### Box Plot SO2
df %>%
  filter(summary != "missing") %>%
  ggplot(aes(y=SO2,fill=summary)) + 
  geom_boxplot() +
  scale_color_brewer(palette = "Dark2", name="Weather Summary") +
  scale_fill_discrete(name="Weather Summary") +
  #theme(legend.position="none") + 
  labs(x = "", y = expression(SO[2]~concentration))

##########################################################################################
# Hourly distibution
##########################################################################################

### Box Plot PM2.5
df %>%
  mutate(Hour = as.factor(Hour)) %>%
  ggplot(aes(y=PM2.5,x=Hour)) + 
  geom_boxplot() +
  #scale_fill_brewer(palette = "Dark2", name="Hour") +
  labs(x = "Hour", y = expression(PM[2.5]~concentration))


##########################################################################################
# TimeofDay distibution
##########################################################################################

### Box Plot PM2.5
df %>%
  ggplot(aes(y=PM2.5,fill=TimeofDay)) + 
  geom_boxplot() +
  scale_fill_brewer(palette = "Dark2", name="Time of Day") +
  labs(x = "", y = expression(PM[2.5]~concentration))

##########################################################################################
# DAy of Week distibution
##########################################################################################

### Box Plot PM2.5
df %>%
  ggplot(aes(y=PM2.5,fill=Weekday)) + 
  geom_boxplot() +
  scale_fill_brewer(palette = "Dark2", name="Day of Week") +
  labs(x = "", y = expression(PM[2.5]~concentration))

##########################################################################################
# Weekday Weekend Time of Day distibution
##########################################################################################

### Box Plot PM2.5
df %>%
  mutate(Daytype = factor(Weekend, levels = c(TRUE,FALSE), 
                          labels = c("Weekend", "Weekday"))) %>%
  ggplot(aes(y=PM2.5,fill=TimeofDay)) + 
  geom_boxplot() +
  facet_grid(cols=vars(Daytype)) +
  scale_fill_brewer(palette = "Dark2", name="Time of Day") +
  labs(x = "", y = expression(PM[2.5]~concentration))

##########################################################################################
# Effect of Weather Summary
##########################################################################################

### Scatter Plot
df %>%
  ggplot(aes(x=Timeline, y=PM2.5, color=summary)) + 
  geom_point()+
  scale_color_brewer(palette = "Dark2", name="Weather Summary") +
  labs(x = "Date", y = expression(PM[2.5]~hourly~concentration))

#legend <- get_legend(Plot1)
#Plot1 <- Plot1 + theme(legend.position="none")

### Box Plot PM2.5
df %>%
  filter(summary != "missing") %>%
  ggplot(aes(y=PM2.5,fill=summary)) + 
  geom_boxplot() +
  scale_color_brewer(palette = "Dark2", name="Weather Summary") +
  scale_fill_discrete(name="Weather Summary") +
  #theme(legend.position="none") + 
  labs(x = "", y = expression(PM[2.5]~hourly~concentration))

# create blank plot
blankPlot <- ggplot()+geom_blank(aes(1,1)) + 
  cowplot::theme_nothing()

# Top-right
grid.arrange(Plot1,Plot2,
             nrow = 1, ncol =2,
             #widths = c(2.7, 2.7), heights = c(0.2, 2.5),             
             top = expression(The~Weather~effect~on~hourly~PM[2.5]~concentration))

##########################################################################################
# Precipitation
##########################################################################################

# PrecipProbability Vs PM2.5
Plot1 <- ggplot(df, aes(x = precipProbability, y = PM2.5)) + 
  geom_point(col = "blue", size = 2) + 
  #geom_smooth() +
  labs(x = "Precipitation Probability ", y = expression(PM[2.5]~hourly~concentration)) 

# PrecipIntensity Vs PM2.5
Plot2 <- ggplot(df, aes(x = precipIntensity, y = PM2.5)) + 
  geom_point(col = "dark green", size = 2) + 
  #geom_smooth() +
  labs(x = "Precipitation Intensity", y = expression(PM[2.5]~hourly~concentration)) 

grid.arrange(Plot1,Plot2,
             nrow = 1, ncol =2,
             top = expression(The~Precipitation~effect~on~PM[2.5]~hourly~concentration))

##########################################################################################
# Temperature
##########################################################################################

# temperature Vs PM2.5
Plot1 <- ggplot(df, aes(x = temperature, y = PM2.5)) + 
  geom_point(col = "blue", size = 2) + 
  labs(x = "Temperature ", y = expression(PM[2.5]~hourly~concentration)) 

# apparentTemperature Vs PM2.5
Plot2 <- ggplot(df, aes(x = apparentTemperature, y = PM2.5)) + 
  geom_point(col = "dark green", size = 2) + 
  labs(x = "Apparent Temperature", y = expression(PM[2.5]~hourly~concentration)) 

grid.arrange(Plot1,Plot2,
             nrow = 1, ncol =2,
             top = expression(The~Temperature~effect~on~PM[2.5]~hourly~concentration))

##########################################################################################
# Dew Point Humidity
##########################################################################################

# dewPoint Vs PM2.5
Plot1 <- ggplot(df, aes(x = dewPoint, PM2.5)) + 
  geom_point(col = "blue", size = 2) + 
  labs(x = "Dew Point ", y = expression(PM[2.5]~hourly~concentration)) 

# humidity Vs PM2.5
Plot2 <- ggplot(df, aes(x = humidity, y = PM2.5)) + 
  geom_point(col = "dark green", size = 2) + 
  labs(x = "Humidity", y = expression(PM[2.5]~hourly~concentration)) 

grid.arrange(Plot1,Plot2,
             nrow = 1, ncol =2,
             top = expression(The~Dew~Point~and~Humidity~effect~on~PM[2.5]~hourly~concentration))

##########################################################################################
# Pressure and UV Index
##########################################################################################

# pressure Vs PM2.5
Plot1 <- ggplot(df, aes(x = pressure, PM2.5)) + 
  geom_point(col = "blue", size = 2) + 
  labs(x = "Pressure ", y = expression(PM[2.5]~hourly~concentration)) 

# uvIndex Vs PM2.5
Plot2 <- ggplot(df, aes(x = uvIndex, y = PM2.5)) + 
  geom_point(col = "dark green", size = 2) + 
  labs(x = "UV Index", y = expression(PM[2.5]~hourly~concentration)) 

grid.arrange(Plot1,Plot2,
             nrow = 1, ncol =2,
             top = expression(The~Pressure~and~UV~Index~effect~on~PM[2.5]~hourly~concentration))

##########################################################################################
# Cloud Cover and Visibility
##########################################################################################

# cloudCover Vs PM2.5
Plot1 <- ggplot(df, aes(x = cloudCover, PM2.5)) + 
  geom_point(col = "blue", size = 2) + 
  labs(x = "Cloud Cover ", y = expression(PM[2.5]~hourly~concentration)) 

# visibility Vs PM2.5
Plot2 <- ggplot(df, aes(x = visibility, y = PM2.5)) + 
  geom_point(col = "dark green", size = 2) + 
  labs(x = "Visibility", y = expression(PM[2.5]~hourly~concentration)) 

grid.arrange(Plot1,Plot2,
             nrow = 1, ncol =2,
             top = expression(The~Cloud~Cover~and~Visibility~effect~on~PM[2.5]~hourly~concentration))

##########################################################################################
# Wind Speed and Wind Gust
##########################################################################################

# windSpeed Vs PM2.5
Plot1 <- ggplot(df, aes(x = windSpeed, PM2.5)) + 
  geom_point(col = "blue", size = 2) + 
  labs(x = "Wind Speed ", y = expression(PM[2.5]~hourly~concentration)) 

# windGust Vs PM2.5
Plot2 <- ggplot(df, aes(x = windGust, y = PM2.5)) + 
  geom_point(col = "dark green", size = 2) + 
  labs(x = "Wind Gust", y = expression(PM[2.5]~hourly~concentration)) 

grid.arrange(Plot1,Plot2,
             nrow = 1, ncol =2,
             top = expression(The~Wind~Speed~and~Wind~Gust~effect~on~PM[2.5]~hourly~concentration))


##########################################################################################
# Wind Bearing
##########################################################################################

library(tidyverse)

rose_breaks <- c(0, 360/32, (1/32 + (1:15 / 16)) * 360, 360)
rose_labs <- c(
  "North", "North-Northeast", "Northeast", "East-Northeast",
  "East", "East-Southeast", "Southeast", "South-Southeast",
  "South", "South-Southwest", "Southwest", "West-Southwest",
  "West", "West-Northwest", "Northwest", "North-Northwest",
  "North"
)

df %>%
  mutate(windDirection = cut(windBearing,breaks = rose_breaks,labels = rose_labs,
                             right = FALSE,include.lowest = TRUE)) %>%
  mutate(windDirection = as.factor(windDirection)) %>%
  ggplot(aes(y=PM2.5,fill=windDirection)) + 
  geom_boxplot() +
  labs(x = "", y = expression(PM[2.5]~hourly~concentration))

#Yearly PM2.5 concentration

df %>%
  mutate(wd = windBearing) %>%
  mutate(ws = windSpeed) %>%
  mutate(date = Timeline) %>%
  mutate(pm25 = PM2.5) %>%
  polarPlot(pollutant = "pm25", 
            layout = c(1,1), key.header = "Mean of PM2.5",  
            cols = c("#003300","#0000FF", "#FF9933", "#FF36FF", "#FF3300"))

windRose(df, ws="windSpeed", wd="windBearing" )

##########################################################################################
# Correlation
##########################################################################################

corr <- cor(df[,c(4,11:22,24)])
corrplot(corr, method = "circle", tl.col="black", tl.cex=0.8, tl.srt=45)
corrplot(corr, method = "number")

grid.arrange(cplot1,cplot2,
             nrow = 1, ncol =2,
             top = expression(Correlation~Plot~PM[2.5]~hourly~concentration~and~Weather~features))




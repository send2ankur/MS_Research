library(darksky)
library(tidyverse)
library(dplyr)

# Set API Key
#Sys.setenv(DARKSKY_API_KEY = "<Insert Dark Sky API Key")

###########################################
# Historical Forecast Delhi Punjabi Bagh
###########################################

datelistdlp <- seq(as.Date("2018-01-01"), as.Date("2019-12-31"), by="days")
datelistdlp <- paste(datelistdlp,"T00:00:00",sep="")
thendlp <- get_forecast_for(28.6707, 77.1285, "2018-01-01T00:00:00", units="si",add_headers=TRUE)
dlp_wdfh <- thendlp$hourly
dlp_wdfd <- thendlp$daily
for(i in 2:length(datelistdlp)){
  then_temp <- get_forecast_for(28.6707, 77.1285, datelistdlp[i], units="si",add_headers=TRUE)
  dlp_wdfh <- bind_rows(dlp_wdfh,then_temp$hourly)
  dlp_wdfd <- bind_rows(dlp_wdfd,then_temp$daily)
}

write.csv(dlp_wdfh,"delpb_wdfh.csv")
write.csv(dlp_wdfd,"delpb_wdfd.csv")

### 2020 data

datelistdlp <- seq(as.Date("2020-01-01"), as.Date("2020-02-10"), by="days")
datelistdlp <- paste(datelistdlp,"T00:00:00",sep="")
thendlp <- get_forecast_for(28.6707, 77.1285, "2020-01-01T00:00:00", units="si",add_headers=TRUE)
dlp_wdfh <- thendlp$hourly
dlp_wdfd <- thendlp$daily
for(i in 2:length(datelistdlp)){
  then_temp <- get_forecast_for(28.6707, 77.1285, datelistdlp[i], units="si",add_headers=TRUE)
  dlp_wdfh <- bind_rows(dlp_wdfh,then_temp$hourly)
  dlp_wdfd <- bind_rows(dlp_wdfd,then_temp$daily)
}

write.csv(dlp_wdfh,"delpb_wdfh_2020.csv")
write.csv(dlp_wdfd,"delpb_wdfd_2020.csv")

############################################
# Historical Forecast Vasundhara Ghaziabad
############################################

datelistgha <- seq(as.Date("2018-01-01"), as.Date("2019-12-31"), by="days")
datelistgha <- paste(datelistgha,"T00:00:00",sep="")
thengha <- get_forecast_for(28.6624, 77.3734, "2018-01-01T00:00:00", units="si",add_headers=TRUE)
gha_wdfh <- thengha$hourly
gha_wdfd <- thengha$daily
for(i in 2:length(datelistgha)){
  then_temp <- get_forecast_for(28.6624, 77.3734, datelistgha[i], units="si",add_headers=TRUE)
  gha_wdfh <- bind_rows(gha_wdfh,then_temp$hourly)
  gha_wdfd <- bind_rows(gha_wdfd,then_temp$daily)
}

write.csv(gha_wdfh,"gha_wdfh.csv")
write.csv(gha_wdfd,"gha_wdfd.csv")

### 2020 data

datelistgha <- seq(as.Date("2020-01-01"), as.Date("2020-02-10"), by="days")
datelistgha <- paste(datelistgha,"T00:00:00",sep="")
thengha <- get_forecast_for(28.6624, 77.3734, "2020-01-01T00:00:00", units="si",add_headers=TRUE)
gha_wdfh <- thengha$hourly
gha_wdfd <- thengha$daily
for(i in 2:length(datelistgha)){
  then_temp <- get_forecast_for(28.6624, 77.3734, datelistgha[i], units="si",add_headers=TRUE)
  gha_wdfh <- bind_rows(gha_wdfh,then_temp$hourly)
  gha_wdfd <- bind_rows(gha_wdfd,then_temp$daily)
}

write.csv(gha_wdfh,"gha_wdfh_2020.csv")
write.csv(gha_wdfd,"gha_wdfd_2020.csv")


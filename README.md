# MS_Research
This study explores daily and seasonal trends in air pollution across Delhi and Ghaziabad in India, and the impact of weather on these trends. The research explores air quality prediction model based solely on meteorological data to forecast hourly PM2.5 concentration. As air quality and meteorological data is available in the form of time series, ARIMAX is used as the base forecasting method, and is then compared with multiple linear regression, support vector regression, random forest and extreme gradient boosting methods. A sliding window mechanism is employed to convert the data into a suitable format for classical supervised machine learning and helps to explore temporal features as well. The forecasts using various methods are compared with the data observed at Continuous Ambient Air Quality Monitoring Stations and the experimental results show that SVM and Random Forest methods are superior to others. 

Data Source:
1. Air Quality data available at Central Control Room for Air Quality Management and maintained by CPCB (https://app.cpcbccr.com/ccr/#/caaqm-dashboard-all/caaqm-landing)
2. Historical and forecasted weather data Powered By Dark Sky 
(https://darksky.net/poweredby/)

Datasets:
1. Daily.xlsx - Daily averaged air quality data from 1st Jan 2018 - 31st Dec 2019
2. Hourly.xlsx - Hourly averaged air quality data from 1st Jan 2018 - 10th Feb 2020
3. delpb_wdfd.csv - Daily observed weather data from 1st Jan 2018 - 31st Dec 2019 in Delhi (Punjabi Bagh)
4. delpb_wdfh.csv - Hourly observed weather data from 1st Jan 2018 - 31st Dec 2019 in Delhi (Punjabi Bagh)
5. delpb_wdfh_2020.csv - Hourly observed weather data from 1st Jan 2018 - 10th Feb 2020 in Delhi (Punjabi Bagh)
6. gha_wdfd.csv - Daily observed weather data from 1st Jan 2018 - 31st Dec 2019 in Ghaziabad (Vasundhara)
7. gha_wdfh.csv - Hourly observed weather data from 1st Jan 2018 - 31st Dec 2019 in Ghaziabad (Vasundhara)
8. gha_wdfh_2020.csv - Hourly observed weather data from 1st Jan 2020 - 10th Feb 2020 in Ghaziabad (Vasundhara)

Description of files, along with order of execution
0. DarkSky.R -- Optional step to retrieve data from DarK Sky API as the data is already available in repository
1. Daily_dataprep.R -- Standard data preparation for Daily air quality and meteorological data 
2. Hourly_dataprep.R -- Standard data preparation for Hourly air quality and meteorological data 
3. Final_Plots.R -- Exploratory data analysis of Daily and Hourly data 
4. TimeSeries_Models.R -- ARIMAX model for hourly PM2.5 forecasting
5. Final_Models.R -- MLR, SVM, Random Forest and XGBoost models for hourly PM2.5 forecasting
6. Final_Results.R -- Evaluate forecast models against observed data from 1st Jan 2020 - 15th Jan 2020 and analyze the fit

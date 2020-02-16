# MS_Research
This study explores daily and seasonal trends in air pollution across Delhi and Ghaziabad in India, and the impact of weather on these trends. The research explores air quality prediction model based solely on meteorological data to forecast hourly PM2.5 concentration. As air quality and meteorological data is available in the form of time series, ARIMAX is used as the base forecasting method, and is then compared with multiple linear regression, support vector regression, random forest and extreme gradient boosting methods. A sliding window mechanism is employed to convert the data into a suitable format for classical supervised machine learning and helps to explore temporal features as well. The forecasts using various methods are compared with the data observed at Continuous Ambient Air Quality Monitoring Stations and the experimental results show that SVM and Random Forest methods are superior to others. 

library(dplyr)
library(e1071)
library(xgboost)
library(ranger)
library(forecast)
library(tseries)
library(ggplot2)
## ggplot theme setting
theme_set(theme_bw())
library(grid)
library(gridExtra)
library(Ckmeans.1d.dp)


trainf <- readRDS("trainf_pw.rds")
vldtnf <- readRDS("vldtnf_pw.rds")
testf <- readRDS("testf_pw.rds")
scales <- readRDS("scales.rds")

trainw0 <- trainf[,-c(1,52:55)]
vldtnw0 <- vldtnf[,-c(1,52:55)]
testw0 <- testf[,-c(1,52:55)]

############################################################
### MLR
############################################################

lmw0_19 <- lm(formula = pm25_0 ~ summary.xclear.night + summary.xcloudy + 
                summary.xfog + summary.xmissing + summary.xpartly.cloudy.day + 
                summary.xpartly.cloudy.night + summary.xrain + Weekday.xSunday + 
                Weekday.xThursday + TimeofDay.xEarlyMorning + TimeofDay.xMorning + 
                TimeofDay.xNight + City + wd.xEast.Southeast + wd.xNorth.Northwest + 
                wd.xNorthwest + wd.xSouth.Southeast + wd.xSouthwest + wd.xWest + 
                wd.xWest.Northwest + windS_0 + visibility_0 + Day + precipP_4 + 
                apptemp_1 + dewP_4 + windS_2 + windS_4 + cloudcover_3 + precipI.3 + 
                precipI.5 + precipP.4 + +dewP.7 + pressure.4 + windS.7 + 
                cloudcover.5 + uvIndex.7 + windG.2 + windG.5 + windG.7, data = trainw0)
saveRDS(lmw0_19,"C:/Users/send2/Documents/Ankur/MSc/Temp/mlModel/lmw0_19.rds")

# Validation
lm_predvl0 <- predict(lmw0_19,vldtnw0)
accuracy(lm_predvl0,vldtnw0$pm25_0)
#                  ME     RMSE      MAE       MPE     MAPE
# Test set -0.3990567 68.63901 50.14912 -42.53228 82.61603

# Test
lm_predts0 <- predict(lmw0_19,testw0)
lmw019_eval <- data.frame(Timeline=as.POSIXct(testf$Timeline), 
                          Forecast=lm_predts0, 
                          Actual=testw0$pm25_0)
saveRDS(lmw019_eval,"lmw019_eval.rds")
#lmw019_eval <- readRDS("lmw019_eval.rds")

# Delhi
accuracy(lm_predts0[1:360],testw0$pm25_0[1:360])
#                 ME     RMSE      MAE       MPE     MAPE
# Test set  7.380213 85.82019 66.25825 -23.26872 48.98747

# Ghaziabad
accuracy(lm_predts0[738:1097],testw0$pm25_0[738:1097])
#                 ME     RMSE      MAE       MPE     MAPE
# Test set   28.7586 89.20349 67.10028 -2.925029 35.69303

############################################################
### SVM
############################################################

svmw0_1 <- svm(pm25_0~., data = trainw0)
saveRDS(svmw0_1,"C:/Users/send2/Documents/Ankur/MSc/Temp/mlModel/svmw0_1.rds")

# Validation
svm_predvl0 <- predict(svmw0_1,vldtnw0)
accuracy(svm_predvl0,vldtnw0$pm25_0)
#                  ME     RMSE      MAE       MPE     MAPE
# Test set 3.943303 48.01285 29.46566 -20.03319 39.81081

# Test
svm_predts0 <- predict(svmw0_1,testw0)
svmw01_eval <- data.frame(Timeline=as.POSIXct(testf$Timeline), 
                          Forecast=svm_predts0, 
                          Actual=testw0$pm25_0)
saveRDS(svmw01_eval,"svmw01_eval.rds")

# Delhi
accuracy(svm_predts0[1:360],testw0$pm25_0[1:360])
#                 ME     RMSE      MAE       MPE     MAPE
# Test set  -11.7207 74.83348 60.49491 -30.36274 51.48081

# Ghaziabad
accuracy(svm_predts0[738:1097],testw0$pm25_0[738:1097])
#                 ME     RMSE      MAE       MPE     MAPE
# Test set   8.494447 84.39917 65.25453 -12.20479 41.28016

############################################################
### RANDOM FOREST
############################################################

rfw0_2 <- ranger(formula=pm25_0~., data=trainw0, num.trees=500, 
              mtry=30, min.node.size=3, sample.fraction=0.8,
              importance="impurity")
saveRDS(rfw0_2,"C:/Users/send2/Documents/Ankur/MSc/Temp/mlModel/rfw0_2.rds")

# Validation
rf_predvl0 <- predict(rfw0_2,vldtnw0)
accuracy(rf_predvl0$predictions,vldtnw0$pm25_0)
#                  ME     RMSE      MAE       MPE     MAPE
# Test set -1.443632 41.75869 26.74187 -29.06648 42.6615

# Test
rf_predts0 <- predict(rfw0_2,testw0)
rfw02_eval <- data.frame(Timeline=as.POSIXct(testf$Timeline), 
                          Forecast=rf_predts0$predictions, 
                          Actual=testw0$pm25_0)
saveRDS(rfw02_eval,"rfw02_eval.rds")

# Delhi
accuracy(rf_predts0$predictions[1:360],testw0$pm25_0[1:360])
#                 ME     RMSE      MAE       MPE     MAPE
# Test set  -36.84726 84.92946 72.93485 -53.30114 65.3867

# Ghaziabad
accuracy(rf_predts0$predictions[738:1097],testw0$pm25_0[738:1097])
#                 ME     RMSE      MAE       MPE     MAPE
# Test set   -1.62508 76.2189 58.6343 -18.25674 37.24901


### Variable Importance

v <- as.vector(as.numeric(rfw0_2$variable.importance))
w <- as.vector(colnames(testw0[,-c(1)]))
DF <-as.data.frame(cbind(w,v))
DF$v <- as.numeric(DF$v)
DF <- arrange(DF, desc(v))

ggplot(DF[1:20,], aes(x=reorder(w,v), y=v,fill=v))+ 
  geom_bar(stat="identity", position="dodge")+ coord_flip()+
  ylab("Variable Importance")+
  xlab("")+
  ggtitle("Information Value Summary")+
  guides(fill=F)+
  scale_fill_gradient(low="red", high="blue")

############################################################
### XGBOOST
############################################################

# Preparing matrix 
tr_label <- trainw0$pm25_0
dtrain <- xgb.DMatrix(data = as.matrix(trainw0[,-c(1)]),label = tr_label)
vl_label <- vldtnw0$pm25_0
dvldtn <- xgb.DMatrix(data = as.matrix(vldtnw0[,-c(1)]),label = vl_label)
ts_label <- testw0$pm25_0
dtest <- xgb.DMatrix(data = as.matrix(testw0[,-c(1)]), label = ts_label)

# Parameter List
params <- list(booster = "gbtree", objective = "reg:squarederror", eta=0.01, gamma=0, 
               max_depth=5, min_child_weight=1, subsample=1, colsample_bytree=1)

# Build Model
set.seed(5001)
xgbw0_2 <- xgb.train (params = params, data = dtrain, nrounds = 1000)
saveRDS(xgbw0_2,"C:/Users/send2/Documents/Ankur/MSc/Temp/mlModel/xgbw0_2.rds")

# Validation
xgb_predvl0 <- predict(xgbw0_2,dvldtn)
accuracy(xgb_predvl0,vl_label)
#                  ME     RMSE      MAE       MPE     MAPE
# Test set -0.06132655 45.94852 30.5734 -32.50549 50.27847

# Test
xgb_predts0 <- predict(xgbw0_2,dtest)
xgbw02_eval <- data.frame(Timeline=as.POSIXct(testf$Timeline), 
                            Forecast=xgb_predts0, 
                            Actual=testw0$pm25_0)
saveRDS(xgbw02_eval,"xgbw02_eval.rds")

# Delhi
accuracy(xgb_predts0[1:360],ts_label[1:360])
#                 ME     RMSE      MAE       MPE     MAPE
# Test set -47.63207 88.92789 73.98196 -58.62309 68.3309

# Ghaziabad
accuracy(xgb_predts0[738:1097],ts_label[738:1097])
#                 ME     RMSE      MAE      MPE     MAPE
# Test set -34.86073 88.37206 69.27442 -36.13041 48.41607

mat <- xgb.importance (feature_names = colnames(trainw0[,-c(1)]),model = xgbw0_2)
xgb.ggplot.importance(importance_matrix = mat[1:20], rel_to_first = TRUE)



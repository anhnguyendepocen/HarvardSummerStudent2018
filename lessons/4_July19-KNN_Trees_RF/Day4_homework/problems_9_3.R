
library(caret)
library(MLmetrics)
library(rpart)
library(rpart.plot)

setwd('~/HarvardSummerStudent2018/book datasets')

df <- read.csv('ToyotaCorolla.csv')

## Create Binned_Price column

df$Binned_Price <- ((max(df$Price) - min(df$Price)) / 20) * (.5 + df$Price %/% ((max(df$Price) - min(df$Price)) / 20))

## Split dataset 60/40%

set.seed(1)
df.training.n <- round(nrow(df) %*% .6)
df.training.idx <- sample(1:nrow(df), df.training.n)

df.training <- df[df.training.idx,]
df.validation <- df[-df.training.idx,]

## Train Decision Tree model

fit.rpart.overfit <- train(Price ~ Age_08_04 + KM + Fuel_Type + HP + Automatic + Doors + Quarterly_Tax + Mfr_Guarantee + Guarantee_Period + Airco + Automatic_airco + CD_Player + Powered_Windows + Sport_Model + Tow_Bar,
                           data = df.training,
                           method = 'rpart',
                           tuneGrid = data.frame(cp = c(-1)),
                           control = rpart.control(minsplit = 1, minbucket = 1))

# fit.rpart.overfit; prp(fit.rpart.overfit$finalModel, extra = 1)

## Mesure fit.rpart.overfit performance against training and validation model

MLmetrics::RMSE(predict(fit.rpart.overfit, df.training), df.training$Price)
hist(predict(fit.rpart.overfit, df.training) - df.training$Price)

MLmetrics::RMSE(predict(fit.rpart.overfit, df.validation), df.validation$Price)
MLmetrics::MAPE(predict(fit.rpart.overfit, df.validation), df.validation$Price)
MLmetrics::MAE(predict(fit.rpart.overfit, df.validation), df.validation$Price)
hist(predict(fit.rpart.overfit, df.validation) - df.validation$Price)

## Train non-overfit Decision Tree model

fit.rpart <- train(Price ~ Age_08_04 + KM + Fuel_Type + HP + Automatic + Doors + Quarterly_Tax + Mfr_Guarantee + Guarantee_Period + Airco + Automatic_airco + CD_Player + Powered_Windows + Sport_Model + Tow_Bar,
                   data = df.training,
                   method = 'rpart',
                   tuneGrid = data.frame(cp = seq(0.0001, 0.01, 0.0001)),
                   control = rpart.control(minsplit = 1, minbucket = 1))

fit.rpart; prp(fit.rpart$finalModel, extra = 1)

## Mesure fit.rpart performance against training and validation model

MLmetrics::RMSE(predict(fit.rpart, df.training), df.training$Price)
hist(predict(fit.rpart, df.training) - df.training$Price)

MLmetrics::RMSE(predict(fit.rpart, df.validation), df.validation$Price)
MLmetrics::MAPE(predict(fit.rpart, df.validation), df.validation$Price)
MLmetrics::MAE(predict(fit.rpart, df.validation), df.validation$Price)
hist(predict(fit.rpart, df.validation) - df.validation$Price)

## Train Decision Tree model on Binned_Price

fit.rpart.binned <- train(Binned_Price ~ Age_08_04 + KM + Fuel_Type + HP + Automatic + Doors + Quarterly_Tax + Mfr_Guarantee + Guarantee_Period + Airco + Automatic_airco + CD_Player + Powered_Windows + Sport_Model + Tow_Bar,
                   data = df.training,
                   method = 'rpart',
                   tuneGrid = data.frame(cp = seq(0.0001, 0.01, 0.0001)),
                   control = rpart.control(minsplit = 1, minbucket = 1))

fit.rpart.binned; prp(fit.rpart.binned$finalModel, extra = 1)

## Compare Binned_Price and Price models

hist(predict(fit.rpart, df.training) - predict(fit.rpart.binned, df.training))

## Mesure fit.rpart performance against training and validation model

MLmetrics::RMSE(predict(fit.rpart.binned, df.training), df.training$Price)
hist(predict(fit.rpart.binned, df.training) - df.training$Price)

MLmetrics::RMSE(predict(fit.rpart.binned, df.validation), df.validation$Price)
MLmetrics::MAPE(predict(fit.rpart.binned, df.validation), df.validation$Price)
MLmetrics::MAE(predict(fit.rpart.binned, df.validation), df.validation$Price)
hist(predict(fit.rpart.binned, df.validation) - df.validation$Price)

## Predict record from b.ii.

predict(fit.rpart,        data.frame(Age_08_04 = 77, KM = 117000, Fuel_Type = 'Petrol', HP = 110, Automatic = 0, Doors = 5, Quarterly_Tax = 100, Mfr_Guarantee = 0, Guarantee_Period = 3, Airco = 1, Automatic_airco = 0, CD_Player = 0, Powered_Windows = 0, Sport_Model = 0, Tow_Bar = 1))
predict(fit.rpart.binned, data.frame(Age_08_04 = 77, KM = 117000, Fuel_Type = 'Petrol', HP = 110, Automatic = 0, Doors = 5, Quarterly_Tax = 100, Mfr_Guarantee = 0, Guarantee_Period = 3, Airco = 1, Automatic_airco = 0, CD_Player = 0, Powered_Windows = 0, Sport_Model = 0, Tow_Bar = 1))

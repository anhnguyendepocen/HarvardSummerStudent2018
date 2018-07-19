
library(MLmetrics)

options(scipen=999)
set.seed(1)

setwd('~/HarvardSummerStudent2018/book datasets')

df <- read.csv('BostonHousing.csv')
df$CAT..MEDV <- NULL

df.training.n <- round(nrow(df) %*% .8)
df.training.idx <- sample(1:nrow(df), df.training.n)

df.training <- df[df.training.idx,]
df.validation <- df[-df.training.idx,]

lm <- lm(MEDV ~ CRIM + CHAS + RM, df.training)
summary(lm)

lm.all <- lm(MEDV ~ ., df.training)
summary (lm.all)

df.validation.predicted <- predict(lm.all, df.validation)
MLmetrics::MedianAE(df.validation.predicted, df.validation$MEDV)

heatmap(cor(df.training[, !(names(df.training) %in% c('MEDV'))]), symm = TRUE, col = cm.colors(20))

df.training.trimmed <- df.training[, !(names(df.training) %in% c('INDUS', 'NOX', 'DIS'))]
heatmap(cor(df.training.trimmed[, !(names(df.training.trimmed) %in% c('MEDV'))]), symm = TRUE, col = cm.colors(20))

lm.trimmed <- lm(MEDV ~ ., df.training.trimmed)
summary (lm.trimmed)

## Forward

lm.forward <- step(lm.all, direction = 'forward')
summary (lm.forward)

df.validation.forwardPredicted <- predict(lm.forward, df.validation)

MLmetrics::RMSE(df.validation.forwardPredicted, df.validation$MEDV)
MLmetrics::MAPE(df.validation.forwardPredicted, df.validation$MEDV)
MLmetrics::MedianAE(df.validation.forwardPredicted, df.validation$MEDV)

## Backward

lm.backward <- step(lm.all, direction = 'backward')
summary (lm.backward)

df.validation.backwardPredicted <- predict(lm.backward, df.validation)

MLmetrics::RMSE(df.validation.backwardPredicted, df.validation$MEDV)
MLmetrics::MAPE(df.validation.backwardPredicted, df.validation$MEDV)
MLmetrics::MedianAE(df.validation.backwardPredicted, df.validation$MEDV)

## Both

lm.both <- step(lm.all, direction = 'both')
summary (lm.both)

df.validation.bothPredicted <- predict(lm.both, df.validation)

MLmetrics::RMSE(df.validation.bothPredicted, df.validation$MEDV)
MLmetrics::MAPE(df.validation.bothPredicted, df.validation$MEDV)
MLmetrics::MedianAE(df.validation.bothPredicted, df.validation$MEDV)

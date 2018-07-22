
library('MLmetrics')
library('vtreat')
library('leaps')

options(scipen=999)
set.seed(1)

setwd('~/HarvardSummerStudent2018/book datasets')

df <- read.csv('Airfares.csv')
df$S_CODE[df$S_CODE == '*'] <- NA
df$E_CODE[df$E_CODE == '*'] <- NA

dp <- designTreatmentsN(df, names(df)[5:17], names(df)[18])
df.treated <- prepare(dp, df)

cor(df.treated[,!(names(df.treated) %in% c('FARE'))], df.treated$FARE)

tapply(df$FARE, df$VACATION, mean)
tapply(df$FARE, df$SW, mean)
tapply(df$FARE, df$SLOT, mean)
tapply(df$FARE, df$GATE, mean)

df.training.n <- round(nrow(df) %*% .8)
df.training.idx <- sample(1:nrow(df), df.training.n)

df.training <- df.treated[df.training.idx,]
df.validation <- df.treated[-df.training.idx,]

lm <- lm(FARE ~ ., df.training)
lm <- step(lm)
lm

lm.exhaustive <- regsubsets(FARE ~ ., data = df.training, method = 'exhaustive')
lm.exhaustive.errors <- rep(NA, 12)
for (i in 1:12) {
  lm.exhaustive.coef <- coef(lm.exhaustive, id = i)
  lm.exhaustive.errors[i] <- mean((df.training$Fare - df.training[, names(lm.exhaustive.coef)] %*% lm.exhaustive.coef) ^ 2)
}
lm.exhaustive

df.validation.predicted <- predict(lm, df.validation)
MLmetrics::RMSE(df.validation.predicted, df.validation$FARE)
MLmetrics::MAPE(df.validation.predicted, df.validation$FARE)
mean(df.validation.predicted - df.validation$FARE)

df.validation.predicted <- # WIP #
MLmetrics::RMSE(df.validation.predicted, df.validation$FARE)
MLmetrics::MAPE(df.validation.predicted, df.validation$FARE)
mean(df.validation.predicted - df.validation$FARE)

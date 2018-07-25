
library(forecast)

setwd('~/HarvardSummerStudent2018/book datasets')

df <- read.csv('ToysRUsRevenues.csv')
df$Q1 <- as.numeric(df$Quarter == 'Q1')
df$Q2 <- as.numeric(df$Quarter == 'Q2')
df$Q3 <- as.numeric(df$Quarter == 'Q3')
df$Q4 <- as.numeric(df$Quarter == 'Q4')

## Split dataset

df.training <- df[1:14,]
df.validation <- df[15:16,]

## Build ts.training and ts.validation timeserie

ts.training <- ts(df.training$Revenue.in.million..., start = c(1992, 1), frequency = 4)
ts.validation <- ts(df.validation$Revenue.in.million..., start = c(1992, 1), frequency = 4)

## Fit Regression model with linear trend and additive seasonality

fit.tslm <- tslm(ts.training ~ trend + season, ts.training) 
summary(fit.tslm)

df.training$predicted <- predict(fit.tslm)

## Plot resulting model

plot(df.training$predicted, col='blue')
lines(df.training$predicted, col='blue')
lines(df.training$Revenue.in.million..., col='red')

## Compute difference in sales between Q1 and Q3

mean(head((df.training$predicted - fit.tslm$coefficients['trend'] * df.training$Index) * df.training$Q3 - 
          (df.training$predicted - fit.tslm$coefficients['trend'] * df.training$Index) * df.training$Q1, 12))

## Compute best selling quarter after adjusting for seasonality

summary(df.training$predicted[df.training$Quarter == 'Q1'])
summary(df.training$predicted[df.training$Quarter == 'Q2'] - (fit.tslm$coefficients['season2'] * df.training$Index[df.training$Quarter == 'Q2']))
summary(df.training$predicted[df.training$Quarter == 'Q3'] - (fit.tslm$coefficients['season3'] * df.training$Index[df.training$Quarter == 'Q3']))
summary(df.training$predicted[df.training$Quarter == 'Q4'] - (fit.tslm$coefficients['season4'] * df.training$Index[df.training$Quarter == 'Q4']))


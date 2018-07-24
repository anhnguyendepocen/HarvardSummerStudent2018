
setwd('~/HarvardSummerStudent2018/book datasets')

df <- read.csv('ToysRUsRevenues.csv')
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

fit.lm <- lm(Revenue.in.million... ~ ., df.training[,!(names(df.training) %in% c('QuarterYear', 'Quarter'))]) 
summary(fit.lm)

df.training$predicted <- predict(fit.lm)

## Plot resulting model

plot(df.training$predicted, col='blue')
lines(df.training$predicted, col='blue')
lines(df.training$Revenue.in.million..., col='red')

## Compute difference in sales between Q1 and Q3

mean(head(df.training$predicted[df.training$Quarter == 'Q3'] - fit.lm$coefficients['Index'] * df.training$Index[df.training$Quarter == 'Q3'], 3) - 
     head(df.training$predicted[df.training$Quarter == 'Q1'] - fit.lm$coefficients['Index'] * df.training$Index[df.training$Quarter == 'Q1'], 3))

## Compute best selling quarter after adjusting for seasonality

summary(df.training$predicted[df.training$Quarter == 'Q1'])
summary(df.training$predicted[df.training$Quarter == 'Q2'] - (fit.lm$coefficients['Q2'] * df.training$Index[df.training$Quarter == 'Q2']))
summary(df.training$predicted[df.training$Quarter == 'Q3'] - (fit.lm$coefficients['Q3'] * df.training$Index[df.training$Quarter == 'Q3']))
summary(df.training$predicted[df.training$Quarter == 'Q4'] - (fit.lm$coefficients['Q4'] * df.training$Index[df.training$Quarter == 'Q4']))

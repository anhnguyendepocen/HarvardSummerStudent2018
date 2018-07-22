
library('MLmetrics')

options(scipen=999)
set.seed(1)

setwd('~/HarvardSummerStudent2018/book datasets')

df <- read.csv('Tayko.csv')
df$sequence_number <- NULL

ggplot(df, aes(Spending, Freq)) +
  geom_point()

ggplot(df, aes(Spending, last_update_days_ago)) +
  geom_point()

df.training.n <- round(nrow(df) %*% .8)
df.training.idx <- sample(1:nrow(df), df.training.n)

df.training <- df[df.training.idx,]
df.validation <- df[-df.training.idx,]

lm <- lm(Spending ~ Freq + last_update_days_ago + Web.order + Gender.male + Address_is_res + US, df.training)
summary(lm)

lm.backward <- step(lm, df.training)

df.validation.predicted <- predict(lm, df.validation)

MLmetrics::RMSE(df.validation.predicted, df.validation$Spending)
MLmetrics::MAPE(df.validation.predicted, df.validation$Spending)
mean(df.validation.predicted - df.validation$Spending)

hist(df.validation.predicted - df.validation$Spending, breaks = 20)

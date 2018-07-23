
## Load libraries
library(caret)
library(FNN)

## Load data
setwd('~/HarvardSummerStudent2018/book datasets')

df <- read.csv('UniversalBank.csv')
df$Education_1 <- as.numeric(df$Education == 1)
df$Education_2 <- as.numeric(df$Education == 2)
df$Education_3 <- as.numeric(df$Education == 3)
df$Education <- NULL

## Split dataset in 60/40%
set.seed(1)
df.training.n <- round(nrow(df) %*% .6)
df.training.idx <- sample(1:nrow(df), df.training.n)

df.training <- df[df.training.idx,]
df.validation <- df[-df.training.idx,]

## Normalize the data
df.normalizing <- preProcess(df[,c('Age', 'Experience', 'Income', 'Family', 'CCAvg', 'Mortgage')], method = c('center', 'scale'))

df.norm <- predict(df.normalizing, df)
df.training.norm <- df[df.training.idx,]
df.validation.norm <- df[-df.training.idx,]

## Train k-NN model
nn <- knn(k = 1,
          train = df.training.norm[,!(names(df.training.norm) %in% c('ID', 'ZIP.Code', 'Personal.Loan'))], cl = df.training.norm[,'Personal.Loan'],
          test = data.frame(Age = 40, Experience = 10, Income = 84, Family = 2, CCAvg = 2, Education_1 = 0, Education_2 = 1, Education_3 = 0, Mortgage = 0, Securities.Account = 0, CD.Account = 0, Online = 1, CreditCard = 1))

## Predict a. customer
df.training[attr(nn, "nn.index"),'Personal.Loan']

## Find best balance
fit.knn <- train(Personal.Loan ~ ., data = df.training.norm[,!(names(df.training.norm) %in% c('ID', 'ZIP.Code'))], method = 'knn', tuneLength = 50);
fit.knn; plot(fit.knn)

## Confusion matrix for best result
table(as.numeric(predict(fit.knn, df.validation.norm[,!(names(df.validation.norm) %in% c('ID', 'ZIP.Code'))]) > .5), df.validation.norm$Personal.Loan)

## Classify customer from d.
as.numeric(predict(fit.knn, data.frame(Age = 40, Experience = 10, Income = 84, Family = 2, CCAvg = 2, Education_1 = 0, Education_2 = 1, Education_3 = 0, Mortgage = 0, Securities.Account = 0, CD.Account = 0, Online = 1, CreditCard = 1)) > .5)

## Split dataset in 50/30/20%

#' Author: Ted Kwartler
#' Data: 6-4-2018
#' Purpose: Load data build a decision tree
#' https://archive.ics.uci.edu/ml/datasets/bank+marketing


## Set the working directory
setwd("~/HarvardSummerAdmin2018/Lessons/July19/Day4_Data") 

options(scipen=999)

## Load the libraries; 1st time use install.packages('ggplot2')
library(caret)
library(rpart.plot) #visualizing

## Bring in some data
dat <- read.csv('bank.csv', sep=';') 

# Partitioning
set.seed(1234)
idx <- createDataPartition(y = dat$y, p= 0.8, list=F)
trainDat <- dat[idx,]
testDat <- dat[-idx,]

# Force a full tree (override default parameters)
overFit <- rpart(y ~ ., data = trainDat, method = "class", minsplit = 1, minbucket = 1, cp=-1)
overFit
prp(overFit, extra = 1)

# Look at training accuracy
trainProbs <- predict(overFit) #original data saved as part of the modeling object

# Get the final class and actuals
trainClass<-data.frame(class=colnames(trainProbs)[max.col(trainProbs)],actual = trainDat$y)

# Confusion Matrix
confMat <- table(trainClass$class,trainClass$actual)
confMat

# Accuracy
sum(diag(confMat))/sum(confMat)

# Now predict on the test set
testProbs <- predict(overFit, testDat)

# Get the final class and actuals
testClass<-data.frame(class=colnames(testProbs)[max.col(testProbs)],actual = testDat$y)

# Confusion Matrix
confMat <- table(testClass$class,testClass$actual)
confMat

# Accuracy
sum(diag(confMat))/sum(confMat)

##########
dat <- read.csv('bank-full.csv', sep=';') # now using full data set to approximate real scenario 


# Partitioning
set.seed(1234)
idx <- createDataPartition(y = dat$y, p= 0.8, list=F)
trainDat <- dat[idx,]
testDat <- dat[-idx,]

# Fit a decision tree with caret; should reweight 1st but not covered in class
set.seed(1234)
fit <- train(y ~., data = trainDat, method = "rpart", 
             tuneGrid = data.frame(cp = c(0.01, 0.05)),
             control = rpart.control(minsplit = 1, minbucket = 2))
# saveRDS(fit,'fullDataTreeFit.rds')
# fit <- readRDS('fullDataTreeFit.rds')
# Examine
fit

plot(fit)
prp(fit$finalModel, extra = 1)

trainCaret<-predict(fit)
confusionMatrix(trainCaret, trainDat$y)

# Now more consistent accuracy!
testCaret<-predict(fit,testDat)
confusionMatrix(testCaret,testDat$y)

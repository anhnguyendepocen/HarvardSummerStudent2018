#' Author: Ted Kwartler
#' Data: 5-29-2018
#' Purpose: Load data build a KNN Predictor

## Set the working directory
setwd("~/HarvardSummerStudent2018/lessons/4_July19-KNN_Trees_RF/Day4_Data")
## Load the libraries; 1st time use install.packages('ggplot2')
library(caret)
library(e1071)
library(MLmetrics)

## Bring in some data
dat<-read.csv('Absenteeism_at_work.csv', sep=';')

# Drop the unique employee ID
dat$ID <- NULL

# Group low supported factor levels
#threshold <- 40
#lowLevels <- names(which(table(dat$Reason.for.absence) < threshold))
#lowLvlChk <- dat$Reason.for.absence %in% lowLevels
#dat$Reason.for.absence[lowLvlChk] <- 'lowFreq'

# Data partitioning
set.seed(1234)
idx<- sample(1:nrow(dat),(0.8 * nrow(dat)) %/% 1)

trainDat <- dat[idx,]
testDat <- dat[-idx,]

# If you didn't scale prior to making a training set you can preProcess using the caret package
knnFit <- train(Absenteeism.time.in.hours ~ ., 
                data = trainDat, 
                method = "knn", 
                preProcess = c("center","scale"), 
                tuneLength = 20)

# Evaluation
knnFit
plot(knnFit)

# training set accuracy
trainPreds<-predict(knnFit,trainDat)
RMSE(trainPreds,trainDat$Absenteeism.time.in.hours)
data.frame(predictions = head(trainPreds, 10),actuals = head(trainDat$Absenteeism.time.in.hours, 10))

# Testing set accuracy
testPreds<-predict(knnFit,testDat)
RMSE(testPreds,testDat$Absenteeism.time.in.hours)
data.frame(predictions = head(testPreds, 10),actuals = head(testDat$Absenteeism.time.in.hours, 10))



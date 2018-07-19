#' Author: Ted Kwartler
#' Data: 5-24-2018
#' Purpose: Load data build a KNN classifier

## Set the working directory
setwd("~/HarvardSummerAdmin2018/Lessons/July19/Day4_Data")

## Load the libraries; 1st time use install.packages('ggplot2')
library(caret)
library(e1071)
library(plyr)

## Bring in some data
dat<-read.csv('Absenteeism_at_work.csv', sep=';')

## Explore to get familiar
# Dimensions
dim(dat)

# Summary Stats
summary(dat)

# Head
head(dat)

# Get the appropriate X predictors & scale
xDat <- dat[,-c(1,2)]

# Scaling; we don't actually use this code since using library(caret) though
xDat <- scale(xDat, scale=T, center=T)

# Examine scaling mean across all columns
summary(xDat)

# Drop the unique employee ID
dat$ID <- NULL

#  Tally reason codes
table(dat$Reason.for.absence)

# Group low supported factor levels
threshold <- 40
lowLevels <- names(which(table(dat$Reason.for.absence) < threshold))
lowLvlChk <- dat$Reason.for.absence %in% lowLevels
dat$Reason.for.absence[lowLvlChk] <- 'lowFreq'

# Data partitioning
set.seed(1234)
idx<- sample(1:nrow(dat),(0.8 * nrow(dat)) %/% 1)

trainDat <- dat[idx,]
testDat <- dat[-idx,]

# If you didn't scale prior to making a training set you can preProcess using the caret package
knnFit <- train(Reason.for.absence ~ ., 
                data = trainDat, 
                method = "knn", 
                preProcess = c("center","scale"), 
                tuneLength = 20)

# Evaluation
knnFit
plot(knnFit)

# training set accuracy
trainClasses<-predict(knnFit,trainDat)
confusionMatrix(trainClasses,trainDat$Reason.for.absence) #predictions then reference (actual)

# Testing set accuracy
testClasses<-predict(knnFit,testDat)
confusionMatrix(testClasses,testDat$Reason.for.absence)

# To see probabilities 
trainProbs <- predict(knnFit, trainDat, type=c('prob'))
head(trainProbs)
names(trainProbs)[max.col(head(trainProbs))]
head(trainClasses)

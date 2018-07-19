#' Author: Ted Kwartler
#' Data: 5-24-2018
#' Purpose: Load data build a KNN classifier

## Set the working directory
setwd("~/HarvardSummerStudent2018/lessons/4_July19-KNN_Trees_RF/Day4_Data")

## Load the libraries
library(caret)
library(e1071)
library(plyr)

## Bring in some data
dat<-read.csv('Absenteeism_at_work_v3.csv')

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

# Data partitioning
set.seed(1234)
splitPercent <- round(nrow(dat) %*% .9)
totalRecords <- 1:nrow(dat)
idx <- sample(totalRecords, splitPercent)

trainDat <- dat[idx,]
testDat <- dat[-idx,]

# If you didn't scale prior to making a training set you can preProcess using the caret package
knnFit <- train(Reason.for.absence ~ ., #similar formula to lm
                data = trainDat, #data input
                method = "knn", #caret has other methods so specify KNN
                preProcess = c("center","scale")) #normalization

# Evaluation
knnFit
plot(knnFit)

# training set accuracy
trainClasses<-predict(knnFit,trainDat)
resultsDF <- data.frame(actual = trainDat$Reason.for.absence, 
                        classes = trainClasses)
head(resultsDF)

confusionMatrix(trainClasses,trainDat$Reason.for.absence) #predictions then reference (actual)

# Testing set accuracy
testClasses<-predict(knnFit,testDat)
confusionMatrix(testClasses,testDat$Reason.for.absence)

# To see probabilities 
trainProbs <- predict(knnFit, trainDat, type=c('prob'))
head(trainProbs)

# What is the column with the maximum value for each record?
topProb <-max.col(head(trainProbs))

# Get the name of the top valued probability
names(trainProbs)[topProb]

# Is this the same as predicting the classes directly?
head(as.character(trainClasses))

# Tie so max.col and predict function chose randomly; could have been different.
trainProbs[4,]

# End

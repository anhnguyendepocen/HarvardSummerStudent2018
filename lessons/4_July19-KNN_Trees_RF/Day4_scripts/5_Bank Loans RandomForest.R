#' Author: Ted Kwartler
#' Data: 6-4-2018
#' Purpose: Load data build a random forest tree
#' https://archive.ics.uci.edu/ml/datasets/bank+marketing


## Set the working directory
setwd("~/HarvardSummerStudent2018/lessons/4_July19-KNN_Trees_RF/Day4_Data")

# Options
options(scipen=999)# no scientific notation

## Load the libraries; 1st time use install.packages('ggplot2')
library(caret)
library(rpart.plot) #visualizing
library(randomForest)
library(MLmetrics)

## Bring in some data
dat <- read.csv('bank-full_v2.csv')

set.seed(1234)
# To save time in class, we are only training on 20% of the data
splitPercent <- round(nrow(dat) %*% .2)
totalRecords <- 1:nrow(dat)
idx <- sample(totalRecords, splitPercent)

trainDat <- dat[idx,]
testDat <- dat[-idx,]

# Unbalanced class
tallyY <- table(dat$y)

tallyY
tallyY[2]/tallyY[1]

# Down sample all those No Values! Other methods here: 
#https://topepo.github.io/caret/subsampling-for-class-imbalances.html
downDat <- downSample(x = dat[, -ncol(dat)], y = dat$y)

# Balanced class
table(downDat$Class) # changed name to class

# Partition DownSampled
set.seed(1234)
idx <- createDataPartition(y = downDat$Class, p= 0.8, list=F)
trainDat <- downDat[idx,]
testDat <- downDat[-idx,]

# Fit a down sampled model with Caret
downSampleFit <- train(Class ~ .,
                      data = trainDat,
                      method = "rf",
                      verbose = FALSE,
                      ntree = 3,tuneGrid = data.frame(mtry = 1))

predProbs <- predict(downSampleFit, trainDat, type = c("prob"))
predClasses <-predict(downSampleFit, trainDat)


# Compare to a single tree ~88% Accuracy
downSampleFit

# Confusion Matrix
trainClass<-predict(downSampleFit, trainDat)
confusionMatrix(trainClass, trainDat$Class)


# Other interesting model artifacts
varImp(downSampleFit)
plot(varImp(downSampleFit), top = 20)

# See an example tree; but can be a weak one
getTree(downSampleFit$finalModel,1, labelVar=TRUE)

# Add more trees to the forest with the randomForest package (caret takes a long time bc its more thorough)
moreVoters<-randomForest(Class ~ .,data = trainDat, ntree=500)

# Confusion Matrix, compare to 5 trees ~66% accuracy
trainClass<-predict(moreVoters)
confusionMatrix(trainClass, trainDat$Class)

# Review a tree
head(getTree(moreVoters,1, labelVar=TRUE),27)

# Look at improved var importance
varImpPlot(moreVoters)

# Out of Bag OOB= avg prediction error on each training sample using trees that weren't built with those records (similar to a validation)
#https://en.wikipedia.org/wiki/Out-of-bag_error

# plot the RF with a legend
# https://stackoverflow.com/questions/20328452/legend-for-random-forest-plot-in-r
layout(matrix(c(1,2),nrow=1),
       width=c(4,1)) 
par(mar=c(5,4,4,0)) #No margin on the right side
plot(moreVoters, log="y")
par(mar=c(5,0,4,2)) #No margin on the left side
plot(c(0,1),type="n", axes=F, xlab="", ylab="")
legend("top", colnames(moreVoters$err.rate),col=1:4,cex=0.8,fill=1:4)


# Let's optimize # of trees 
someVoters<-randomForest(Class ~ .,data = trainDat, ntree=100)

# Confusion Matrix, compare to 100 trees ~73.8% accuracy
trainClass<-predict(someVoters)
confusionMatrix(trainClass, trainDat$Class)

### Now let's apply to the validation test set
threeVotes <- predict(someVoters, testDat)
fiveHundredVoters <- predict(moreVoters, testDat)
oneHundredVoters <- predict(someVoters,testDat)

# Accuracy Comparison
Accuracy(testDat$Class, threeVotes)
Accuracy(testDat$Class, fiveHundredVoters)
Accuracy(testDat$Class, oneHundredVoters)

# Holdout Data
three <- predict(someVoters, holdoutDat)
fiveHun <- predict(moreVoters, holdoutDat)
oneHun <- predict(someVoters, holdoutDat)

# Accuracy on UNSEEN data
Accuracy(holdoutDat$y, three)
Accuracy(holdoutDat$y, fiveHun)
Accuracy(holdoutDat$y, oneHun)

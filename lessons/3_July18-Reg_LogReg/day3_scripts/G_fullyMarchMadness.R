#' Author: Ted Kwartler
#' Date: 7-6-2018
#' Purpose: Fit a robust logistic regression on basketball data
#' 

# Libs
library(vtreat)
library(MLmetrics)
library(pROC)

# wd
setwd("C:/Users/Edward/Desktop/HarvardSummerAdmin2018/Lessons/3_July18-Reg_LogReg")

# Data
bball <- read.csv('ncaa.csv')

# No Partition, can score the 2013 data for a realistic test set
drops <- c(1,2,47:50)
adminVars <- bball[,drops]
xVars <- bball[,!(names(bball) %in% names(bball)[drops])]

# Design a "C"ategorical variable plan
plan <- designTreatmentsC(xVars, names(xVars)[-45], names(xVars)[45], 1)

# Apply to xVars
treatedX <- prepare(plan, xVars)

# Fit a model
fit <- glm(R1.Class.1.win ~., data = treatedX, family ='binomial')
summary(fit)

# Backward Variable selection to reduce chances of multi-colinearity
# See  ppt appendix for details or chap6
# Takes 5-10m  to run so load a pre-saved copy 
#bestFit <- step(fit, direction='backward')
#saveRDS(bestFit, 'bestFitNCAA.rds')
bestFit <-readRDS('bestFitNCAA.rds')
summary(bestFit)

# Compare model size
length(coefficients(fit))
length(coefficients(bestFit))

# Get predictions
teamPreds <- predict(bestFit, type='response')

# Classify 
cutoff <- 0.5
teamClasses <- ifelse(teamPreds >= cutoff, 1,0)

# Organize w/Actual
results <- data.frame(actual = xVars$R1.Class.1.win,
                      classes = teamClasses)

# Review distribution of probabilities; decent separation
ggplot(results, aes(x=teamPreds, color=as.factor(actual))) +
  geom_density() + geom_vline(aes(xintercept = 0.5), color = 'green')

# Move the cutoff threshold, now more 0 classifications will occur but we get a bit more of the 1's correct.  Its a tradeoff.
ggplot(results, aes(x=teamPreds, color=as.factor(actual))) +
  geom_density() + geom_vline(aes(xintercept = 0.2), color = 'green')

# Get a confusion matrix
(confMat <- ConfusionMatrix(results$classes, results$actual))

# What is the accuracy?
sum(diag(confMat)) / sum(confMat)
Accuracy(results$classes, results$actual)

# ROC curve
(ROCcurve <- roc(results$actual, results$classes))
plot(ROCcurve)

# AUC
AUC(results$classes, results$actual)


# End

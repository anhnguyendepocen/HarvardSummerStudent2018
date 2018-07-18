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
modelingVars <- bball[,!(names(bball) %in% names(bball)[drops])]

# Design a "C"ategorical variable plan
informativeVars <- names(modelingVars)[-45]
target <- names(xVars)[45]

plan <- designTreatmentsC(modelingVars, 
                          informativeVars,
                          target, 1)

# Apply to xVars
treatedX <- prepare(plan, xVars)

# Fit a model
fit <- glm(R1.Class.1.win ~., data = treatedX, family ='binomial')
summary(fit)

# Backward Variable selection to reduce chances of multi-colinearity
# See chap6 for an explanation
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
head(results)

# Get a confusion matrix
(confMat <- ConfusionMatrix(results$classes, results$actual))

# What is the accuracy?
sum(diag(confMat)) / sum(confMat)

# This is the actual KPI Accuracy not to be confused with the forecast package function accuracy() which the book uses :(
Accuracy(results$classes, results$actual)

# Visually how well did we separate our classes?
ggplot(results, aes(x=teamPreds, color=as.factor(actual))) +
  geom_density() + 
  geom_vline(aes(xintercept = cutoff), color = 'green')


# End

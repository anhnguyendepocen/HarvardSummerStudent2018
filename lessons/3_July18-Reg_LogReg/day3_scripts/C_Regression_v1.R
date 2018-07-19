#' Author: Ted Kwartler
#' Date: 7-5-2018
#' Purpose: Regressions
#' 

# Libs

# Setwd
setwd('~/HarvardSummerStudent2018/lessons/3_July18-Reg_LogReg/day3_data')

# Data
houses <-read.csv('BostonHousing.csv')
houses$CAT..MEDV <-NULL #not used categorical Y var

# Partitioning; get 10% test set
houses.training.n <- round(nrow(houses) %*% .9)
set.seed(1234)
idx <- sample(1:nrow(houses), houses.training.n)

houses.training <- houses[idx, ]
houses.testing <- houses[-idx, ]

# Visualize a relationship; do you see a trend
houses<-houses[order(houses$MEDV), ]
plot(houses$RM, houses$MEDV)

# Let's make up a model; medianValue = 0 + 1*rooms
# This means for every room it adds 1 to the median value
abline(0,1, col='red') #intercept, then slope

# Fit a model (univariate) with no intercept
# The equation of this model is the Y ~ the variable RM and with +0 we are forcing there to be NO beta-naught
fit1 <- lm(MEDV ~ RM + CRIM + 0, houses.training)

# Examine
fit1

# Add the function line
abline(a = 0, #intercept
       b = 3.668, col='red') #slope for every room in an house it adds 3.65 to the median value

# Fit a model with the intercept by removing the +0 in the formula, representing the steady state of median values
fit2 <- lm(MEDV ~ RM + CRIM + INDUS, houses.training)

# Examine
fit2

# Add the function line
abline(a = -33.666, #intercept
       b = 8.957 , col='blue') #slope

# Get some predictions on the training set
preds0 <- houses.training$RM #slope is 1 so beta =1 X the actual value
preds1 <- predict(fit1) 
preds2 <-predict(fit2)

# Examine predictions since this is one of the first times we did predict() & compare to the actual values
head(preds1)
head(preds2)
head(houses.training$MEDV)

# Get sum of squared errors
fit0Err <- (houses.training$MEDV - preds0)^2
fit1Err <- (houses.training$MEDV - preds1)^2
fit2Err <- (houses.training$MEDV - preds2)^2 

sum(fit0Err)
sum(fit1Err)
sum(fit2Err)

# Now validation
preds0 <- houses.testing$RM # Again beta = 1 X actual values
preds1 <- predict(fit1, houses.testing)
preds2 <-predict(fit2, houses.testing)

# Get sum of squared errors
fit0Err <- (houses.testing$MEDV - preds0)^2
fit1Err <- (houses.testing$MEDV - preds1)^2
fit2Err <- (houses.testing$MEDV - preds2)^2 

sum(fit0Err)
sum(fit1Err)
sum(fit2Err)


# End
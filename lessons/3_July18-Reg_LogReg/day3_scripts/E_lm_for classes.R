#' Author: Ted Kwartler
#' Date: 7-4-2018
#' Purpose: What happens when regression is applied to a binary outcome?
#' 

# libs
library(ggplot2)
library(dplyr)

# wd
setwd("C:/Users/Edward/Desktop/HarvardSummerAdmin2018/Lessons/3_July18-Reg_LogReg")

# Data
data('diamonds')

# Convert to binary
diamonds$icedOut <- ifelse(diamonds$price >= 11000,1, 0)
diamonds$price <- NULL

set.seed(1234)
sampDiamonds <- sample_n(diamonds, 10000)

# Remember this?
p <- ggplot(sampDiamonds, aes(carat, icedOut)) +geom_point(alpha=0.2)
p

# Since we see a relationship let's make a linear model to predict prices
fit <- lm(icedOut ~ carat + 0, sampDiamonds)
coefficients(fit)

# Add out model predictions; does this look like a good fit?
p <- p + geom_abline(intercept =  0, slope = 0.1681, color='red')
p

# Suppose you *could* get a 12 carat diamond
hopeDiamond <- data.frame(carat = 12)
gettingCrunk <- predict(fit, hopeDiamond)

# 1= Yes, the diamonds is more than $11k; 0 means no.  What does this mean?  We must have used the wrong method!
gettingCrunk

# End
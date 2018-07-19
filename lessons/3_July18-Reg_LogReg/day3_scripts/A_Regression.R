#' Author: Ted Kwartler
#' Date: 7-4-2018
#' Purpose: Build a regression model
#' 

# libs
library(ggplot2)
library(dplyr)

# wd
setwd("~/HarvardSummerStudent2018/lessons/3_July18-Reg_LogReg")

# Data
data('diamonds')
set.seed(1)

# This is a simple down-sample, not a partitioning schema.  
# There is a difference because you would resample and get the same rows. 
# When you partition you want to ensure no overlap of records.
# diamonds <- sample_n(diamonds, 10000)

# EDA
summary(diamonds)

# Remember this?
p <- ggplot(diamonds, aes(carat, price)) +geom_point(alpha=0.02)
p

# Since we see a relationship let's make a linear model to predict prices
fit <- lm(price ~ carat + 0, diamonds)
fit

# Add out model predictions
p <- p + geom_abline(intercept =  0, slope = 5666, color='red')
p

# End

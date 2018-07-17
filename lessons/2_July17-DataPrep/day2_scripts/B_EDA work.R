#' Author: Ted Kwartler
#' Date: 7-4-2018
#' Purpose: Cereal EDA
#' 

# libs
library(radiant.data)
library(DataExplorer)

# WD
setwd("C:/Users/Edward/Desktop/HarvardSummerAdmin2018/Lessons/2_July17-DataPrep")

# Data
cereal <-read.csv('Cereals.csv')

# What's the overall structure  & dimensions of the data?
str(     )
dim(     )

# Data set class
class(     )

# Classes for each column
sapply(cereal, class)

# Look at the top 6 rows
head(     )

# How many differnt brand names?
nlevels(     )

# Summary stats for each vector
summary(      )

# What's the relationship between protein and sugar?
cor(cereal$, cereal$   )

# How many unique manufacturers?
unique(     )

# Avg calories?
mean(     )

# Number missing values?
colSums(is.na(     ))

# DataExplorer
plot_str(cereal)
plot_missing(cereal)
#plot_histogram(cereal) #time consuming w/o more RAM
#plot_density(cereal) #time consuming w/o more RAM
#plot_scatterplot(cereal, by='rating') #time consuming w/o more RAM

# radiant.data
radiant.data::radiant.data()


# End
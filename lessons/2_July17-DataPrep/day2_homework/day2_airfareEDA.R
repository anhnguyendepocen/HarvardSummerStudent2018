#' Author: Ted Kwartler
#' Data: 5-18-2018
#' Student:
#' Assignment: Day2 EDA
#' Instructions: Add your name above, fill out the answers below by adding code, results or values per below.  Save the file with your first and last name in the file title as in Ted_Kwartler_Day2_Homework.R. Once finished email your script file.

# wd
setwd('~/HarvardSummerStudent2018/lessons/2_July17-DataPrep/day2_homework')

# Libs: radiant.data, DataExplorer
library('radiant.data')
library('DataExplorer')

# Data
airfare <- read.csv('Airfares.csv')
airfare$E_CODE[airfare$E_CODE=='*'] <- NA
airfare$S_CODE[airfare$S_CODE=='*'] <- NA

### Perform EDA 
# Review the top 6 rows
head(airfare)

# Get the summary stats
summary(airfare)

# What is the dimensions of the data frame?
dim(airfare)

# What is the number of starting cities? 
nlevels(airfare$S_CITY)

# What is the number of ending cities?
nlevels(airfare$E_CITY)

# Is there a relationship between starting city population and ending city
cor(airfare$S_POP, airfare$E_POP)

# Visualize the relationship between distance & fare.  Use the base function for a scatterplot.
ggplot(airfare) +
  geom_point(aes(DISTANCE, FARE))

# Whats the average fare?
mean(airfare$FARE)

# Whats the min fare?
min(airfare$FARE)

# Whats the max fare?
max(airfare$FARE)

# Calculate the range(), max() - min() of fares
range(airfare$FARE) #OR
max(airfare$FARE) - min(airfare$FARE)

# Using library(DataExplorer) create a plot of the data structure; navigate the visual to examine variable types
DataExplorer::create_report(airfare)

# Create a plot to see % of missing records by variable
DataExplorer::plot_missing(airfare)

# Using radiant.data, create 2 additional visuals exploring distrubtions & relationships.  Click on the *very* tiny download icon in the upper left of the visual to download a copy and send with the script.
radiant.data()

# End
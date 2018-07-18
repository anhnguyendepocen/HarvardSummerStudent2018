#' Author: Ted Kwartler
#' Data: 5-18-2018
#' Student:
#' Assignment: Day2 EDA & prepare a matrix for modeling
#' Instructions: Add your name above, fill out the answers below by adding code, results or values per below.  Save the file with your first and last name in the file title as in Ted_Kwartler_Day2_Homework.R. Once finished email your script file.

# wd
setwd('~/HarvardSummerStudent2018/lessons/2_July17-DataPrep/day2_homework')

# Libs: DataExplorer, vtreat
library('DataExplorer')
library('vtreat')

# Data
diabetes <- read.csv('10k_diabetesAdj.csv')

# Get all classes
sapply(diabetes, class)

### Perform EDA 
# Review the top 6 rows and first 9 columns
diabetes[ 1:6, 1:9]

# Get the summary stats
summary(diabetes)

# What is the dimensions of the data frame?
dim(diabetes)

# Using the data frame `wentHome` what is the cross tally between:
# discharge_disposition_id and readmitted
wentHome <- subset(diabetes, diabetes$discharge_disposition_id=='Discharged to home')

table(droplevels(wentHome$discharge_disposition_id),
      droplevels(as.factor(wentHome$readmitted)))

# Whats the average num_lab_procedures?
mean(diabetes$num_lab_procedures)

# Whats the min num_medications?
min(diabetes$num_medications)

# What does the histogram of time_in_hospital look like? 
DataExplorer::plot_histogram(diabetes$time_in_hospital)

# Using library(DataExplorer) Create a plot to see % of missing records by variable
DataExplorer::plot_missing(diabetes)

# Remove the variable recommended by DataExplorer visual.
diabetes$weight <- NULL

# Remove the text

## Now design a treatment plan to ready this data for modeling
plan <- designTreatmentsC(diabetes, colnames(diabetes)[1:43],'readmitted', 'FALSE')

# Prepare the data
treatedDiabetes <- prepare(plan, diabetes)

# Save a copy to the folder as 'treatedDiabetes.csv'
write.csv(treatedDiabetes, 'treatedDiabetes.csv', row.names=F)

# End
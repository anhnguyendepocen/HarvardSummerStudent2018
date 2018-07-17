#' Author: Ted Kwartler
#' Data: 5-18-2018
#' Student:
#' Assignment: Day2 EDA & prepare a matrix for modeling
#' Instructions: Add your name above, fill out the answers below by adding code, results or values per below.  Save the file with your first and last name in the file title as in Ted_Kwartler_Day2_Homework.R. Once finished email your script file.

# wd


# Libs: DataExplorer, vtreat

# Data
diabetes <- read.csv(_____)

# Get all classes
sapply(diabetes, class)

### Perform EDA 
# Review the top 6 rows and first 9 columns
diabetes[ _:_, _:_]

# Get the summary stats
____(diabetes)

# What is the dimensions of the data frame?
___(diabetes)

# Using the data frame `wentHome` what is the cross tally between:
# discharge_disposition_id and readmitted
wentHome <- subset(diabetes, diabetes$discharge_disposition_id=='Discharged to home')

table(droplevels(______$________),
      droplevels(as.factor(______$______)))

# Whats the average num_lab_procedures?

# Whats the min num_medications?

# What does the histogram of time_in_hospital look like? 

# Using library(DataExplorer) Create a plot to see % of missing records by variable

# Remove the variable recommended by DataExplorer visual.
____$_____ <- NULL

# Remove the text

## Now design a treatment plan to ready this data for modeling
plan <- designTreatmentsC(______, _____(______)[1:43],'readmitted', 'FALSE')

# Prepare the data
treatedDiabetes <- ____(____,____)

# Save a copy to the folder as 'treatedDiabetes.csv'
write.csv(____,_____, row.names=F)

# End
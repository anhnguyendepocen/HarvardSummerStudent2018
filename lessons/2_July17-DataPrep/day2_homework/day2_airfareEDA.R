#' Author: Ted Kwartler
#' Data: 5-18-2018
#' Student:
#' Assignment: Day2 EDA
#' Instructions: Add your name above, fill out the answers below by adding code, results or values per below.  Save the file with your first and last name in the file title as in Ted_Kwartler_Day2_Homework.R. Once finished email your script file.

# wd


# Libs: radiant.data, DataExplorer

# Data
airfare <- read.csv(_____)

### Perform EDA 
# Review the top 6 rows
____(airfare)

# Get the summary stats
____(airfare)

# What is the dimensions of the data frame?
___(airfare)

# What is the number of starting cities? 
nlevels(____)

# What is the number of ending cities?
___(airfare$____)

# Is there a relationship between starting city population and ending city
cor(____, ____   )

# Visualize the relationship between distance & fare.  Use the base function for a scatterplot.

# Whats the average fare?

# Whats the min fare?

# Whats the max fare?

# Calculate the range(), max() - min() of fares
___(___) #OR
___(___) - ___(___)

# Using library(DataExplorer) create a plot of the data structure; navigate the visual to examine variable types

# Create a plot to see % of missing records by variable

# Using radiant.data, create 2 additional visuals exploring distrubtions & relationships.  Click on the *very* tiny download icon in the upper left of the visual to download a copy and send with the script.

# End
#' Author: Ted Kwartler
#' Date: 6-16-2018
#' Purpose: OKCupid Case Supplemental
#' 

# Libs
library(okcupiddata)
library(dplyr)

# Set WD
setwd('C:/Users/Edward/Desktop/HarvardSummerAdmin2018/Lessons/11_Aug1-GroupPPTs/EDA')

# See all files in wd
dir()

# Get the okcupid data as `profiles`
data('profiles')
latlon<- read.csv('LatLon.csv')

##### I would do some basic EDA and plotting of individual vars then move to more complex interactions
table(profiles$orientation)
hist(profiles$age)

##### Example 2 way EDA
table(profiles$age,profiles$orientation)

##### Maybe do some feature enrichment with other file to make a more complete analysis or a map.  Use library(dplyr)'s left_join() function with the "location" variable




# End

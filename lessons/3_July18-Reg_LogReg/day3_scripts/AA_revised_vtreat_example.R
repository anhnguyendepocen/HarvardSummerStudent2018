#' Author: Ted Kwartler
#' Date: 7-10-2018
#' Purpose: Fundraising PreProcessing
#' Notes: http://storm.cis.fordham.edu/~yli/documents/CISC4631Spring16/FinalProjects.pdf

# Setwd
setwd("C:/Users/Edward/Desktop/HarvardSummerStudent2018/lessons/2_July17-DataPrep/day2_data")

# Libs
library(vtreat)
library(dplyr)

# Read in the data
donors<- read.csv('fakeDonorBureau_v2.csv')

# for **categorical** outcomes (will the prospective donor give Y/N)
# Pass in designTreatmentsC(DATA, NAMES OF INFORMATIVE VARS, RESPONSE VAR, SUCCESS CLASS in quotes)
names(donors)
informativeVariables <- names(donors)[3:19]
target <- 'Y1_Donation'
successClass <- 'Yes'
plan <- designTreatmentsC(donors, informativeVariables, target, successClass)

# Apply the plan
treatedData <- prepare(plan, donors)

# Lots more appended vars; still need to drop redundant flags but much faster and robust!
head(treatedData)

# Start over 
rm(list=ls())

# Data
donors<- read.csv('fakeDonorBureau_v2.csv')

# for **numeric** outcomes (how much will the prospective donor give?)
# DATA, NAMES OF INFORMATIVE VARS, RESPONSE VAR
informativeVariables <- names(donors)[3:19]
target <- 'Y2_DonatedAmt'
plan <- designTreatmentsN(donors, informativeVariables,target)

# Apply the plan
treatedData <- prepare(plan, donors)

# Lots more appended vars; still need to drop redundant flags but much faster and robust!
summary(treatedData)

# Start over 
rm(list=ls())

# Data
donors<- read.csv('fakeDonorBureau_v2.csv')

# Fictitious Data Enrichment
thirdPartyData <- data.frame(uniqueID = donors$uniqueID,
                             likesDogs = rep(c('Yes','No'), nrow(donors)%/%2),
                             creditScore = rnorm(nrow(donors),650))

# Reorder to make more realistic
idx <- sample(1:nrow(thirdPartyData), nrow(thirdPartyData))
thirdPartyData <- thirdPartyData[idx, ]
head(thirdPartyData)

# Examine
head(thirdPartyData)

# Perform a join to the existing data
# Bring new data to the 3120 donors
leftData <- left_join(donors, thirdPartyData)
head(leftData)

# End
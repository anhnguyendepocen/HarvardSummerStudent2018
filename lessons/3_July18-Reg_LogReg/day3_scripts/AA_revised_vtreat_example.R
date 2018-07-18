#' Author: Ted Kwartler
#' Date: 7-10-2018
#' Purpose: Fundraising PreProcessing
#' Notes: http://storm.cis.fordham.edu/~yli/documents/CISC4631Spring16/FinalProjects.pdf

# Setwd
setwd("C:/Users/Edward/Desktop/HarvardSummerStudent2018/lessons/2_July17-DataPrep/day2_data")

# Libs
library(vtreat)

# Read in the data
donors<- read.csv('fakeDonorBureau_v2.csv')

# for **categorical** outcomes (will the prospective donor give Y/N)
# Pass in designTreatmentsC(DATA, NAMES OF INFORMATIVE VARS, RESPONSE VAR, SUCCESS CLASS in quotes)
plan <- designTreatmentsC(donors, names(donors)[3:19],'Y1_Donation', 'Yes')

# Apply the plan
treatedData <- prepare(plan, donors)

# Lots more appended vars; still need to drop redundant flags but much faster and robust!
summary(treatedData)

# Start over 
rm(list=ls())

# Data
donors<- read.csv('fakeDonorBureau_v2.csv')

# for **numeric** outcomes (how much will the prospective donor give?)
# DATA, NAMES OF INFORMATIVE VARS, RESPONSE VAR
plan <- designTreatmentsN(donors, names(donors)[3:19],'Y2_DonatedAmt')

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

# Append more records than in original data to make more realistic
newDat <- data.frame(uniqueID = c('Ted','Nora'),
                     likesDogs = c('Yes','Yes'),
                     creditScore = c(742, 783))

thirdPartyData <- rbind(newDat, thirdPartyData)

# Reorder to make more realistic
idx <- sample(1:nrow(thirdPartyData), nrow(thirdPartyData))
thirdPartyData <- thirdPartyData[idx, ]

# Examine
head(thirdPartyData)

# Perform a join to the existing data
# Bring new data to the 3120 donors; no "Ted" and "Nora"
leftData <- left_join(donors, thirdPartyData) 

# End
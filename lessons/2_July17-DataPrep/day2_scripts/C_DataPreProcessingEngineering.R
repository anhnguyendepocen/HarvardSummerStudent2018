#' Author: Ted Kwartler
#' Date: 7-10-2018
#' Purpose: Fundraising PreProcessing
#' Notes: http://storm.cis.fordham.edu/~yli/documents/CISC4631Spring16/FinalProjects.pdf

# Setwd
setwd("C:/Users/Edward/Desktop/HarvardSummerAdmin2018/Lessons/2_July17-DataPrep")

# Libs
library(vtreat)

# Read in the data
donors<- read.csv('fakeDonorBureau_v2.csv')

# Examine; Here you would perform EDA
summary(     )

## Manual clean up 
# Separate variables
RowID <- donors$RowID
uniqueID <- donors$uniqueID
Y1_Donation <- donors$Y1_Donation
Y2_DonatedAmt <- donors$Y2_DonatedAmt
donors[,c('RowID','uniqueID', 'Y1_Donation','Y2_DonatedAmt')] <-NULL

# Separate the factor columns
factorCols <- sapply(donors, is.factor)
factorVars <- donors[, factorCols]

# Can you do the same for numeric columns in a new object called "numVars"?
numCols <- sapply(donors, _______)
numVars <-_________

# Numeric variables NA Flag
numFlags <- sapply(_____,is.na) * 1
colnames(numFlags) <- paste0(colnames(numFlags),'_missingFlag')

# Can you create a categorical missing flag matrix?
factorFlags<- ______
colames(factorFlags) <-_______

# Let's drop the categorical NA; remember this is a design choice
completeChk <- complete.cases(factorVars)
factorVars<-factorVars[______, ]

# For numeric, how about mean imputation
(colAvgs <- sapply(numVars, _____, na.rm=T))

# Manual Mean Imputation
numVars$NUMCHLD[is.na(numVars$NUMCHLD)] <- colAvgs[1]
numVars$INCOME[is.na(numVars$INCOME)] <- colAvgs[2]
# Can you do the rest of the 14 numeric variables?

# Since we dropped categorical rows we need to drop the SAME rows in the numeric variables
# Join all the data back together
processedDonors <- data.frame(RowID[completeChk],
                              uniqueID[completeChk],
                              Y1_Donation[completeChk],
                              Y2_DonatedAmt[completeChk],
                              factorVars,# don't need to remove the NA rows
                              numVars[completeChk,],
                              factorFlags[completeChk,], numFlags[completeChk,])
# New Variables and no NAs
summary(processedDonors)

# Start over 
rm(list=ls())

### Or an automated manner
donors<- read.csv('fakeDonorBureau_v2.csv')

# for **categorical** outcomes (will the prospective donor give Y/N)
# DATA, NAMES OF INFORMATIVE VARS, RESPONSE VAR, SUCCESS CLASS
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

# Bring donors to the 3122 new data points
rightData <- right_join(donors, thirdPartyData) 
rightData[c(2438:2439, 2589:2590),] #NA automatically filled in

# What's in common?
innerData <- inner_join(donors, thirdPartyData) #here identical to leftData

# End
#' Author: Ted Kwartler
#' Date: 6-18-2018
#' Purpose: Lending Club Clean up

# Set WD
setwd('C:/Users/Edward/Desktop/HarvardSummerAdmin2018/Lessons/7_July 25/day7_data/loans')

# Libraries
library(pbapply)
library(data.table)
library(dplyr)
library(fst)

# Download all CSV from https://www.lendingclub.com/info/download-data.action and put into WD.  Will need to delete first row and bottom 2 rows of the files.

# Remove Transitory States of notes
WIPstates <- c('Current', #outcome unknowns
               'In Grace Period', 
               'Late (16-30 days)','Late (31-120 days)',
               '', #data integrity
               'Does not meet the credit policy. Status:Charged Off', #low frequency
               'Does not meet the credit policy. Status:Fully Paid',
               'Default')


# List & read files
tmp <- list.files('.', pattern = 'Loan')
allLoans<-list()
for (i in 1:length(tmp)){
  x<-fread(tmp[i])
  x <- x[!(x$loan_status %in% WIPstates),]
  nam <- tmp[i]
  allLoans[[nam]] <- x 
  
}

allLoans <- rbindlist(allLoans)

# Outcomes of loans
table(allLoans$loan_status)

# Get originating loan columns
keeps <-names(read.csv('C:/Users/Edward/Desktop/HarvardSummerAdmin2018/Lessons/7_July 25/day7_data/OpenNotes 6-18.csv'))

# Append the outcome variable
keeps <-c(keeps, 'loan_status')

keeps<-keeps[(keeps %in% names(allLoans))]

# Reduce to relevant features
allLoans <- allLoans[,keeps, with=F]

# Make the Y and remove ID vars
allLoans$y <- ifelse(allLoans$loan_status== 'Charged Off',0,1)

drops <- c('loan_status','id', 'member_id')
allLoans <- allLoans[,!(drops), with=F]

# Save
write_fst(allLoans, 'C:/Users/Edward/Desktop/HarvardSummerAdmin2018/Lessons/7_July 25/day7_data/LC/cleanedLoans.fst', compress=100)

#write.csv(allLoans, 'C:/Users/Edward/Desktop/HarvardSummerAdmin2018/Lessons/7_July 25/day7_data/LC/cleanedLoans.csv', row.names=F)

# Sample
set.seed(1234)
sampleLoans <- dplyr::sample_n(allLoans, 20000)
write.csv(sampleLoans,'C:/Users/Edward/Desktop/HarvardSummerAdmin2018/Lessons/7_July 25/day7_data/LC/20K_sampleLoans.csv', row.names = F)
# End
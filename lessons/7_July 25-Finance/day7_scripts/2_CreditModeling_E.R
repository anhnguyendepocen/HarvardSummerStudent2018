#' Author: Ted Kwartler
#' Date: 6-18-2018
#' Purpose: Lending Club Modeling Best Algo, Visualize, and Score then Buy Notes
#'

# Options
options(scipen=999)

# libs
library(caret)
library(e1071)
library(vtreat)
library(dplyr)
library(rbokeh)

# Data Directory
setwd('C:/Users/Edward/Desktop/HarvardSummerAdmin2018/Lessons/7_July 25-Finance/day7_data/LC')

# Data
originalNotes <-read.csv("20K_sampleLoans.csv")
allNewNotes <- read.csv("OpenNotesJune18.csv")

# Keep the pertinent information; as you explore you can add others or delete
keeps <-c("loan_amnt", "term", "int_rate", "installment", "grade", "sub_grade", "emp_length" , "home_ownership", "annual_inc", "purpose", "title", "zip_code", "addr_state", "dti", "delinq_2yrs", "pub_rec_bankruptcies", "inq_last_6mths", "mths_since_last_delinq", "mths_since_last_record", "open_acc", "pub_rec", "revol_bal", "revol_util", "total_acc", "initial_list_status", "collections_12_mths_ex_med", "mths_since_last_major_derog","y")

originalNotes <- originalNotes[,keeps]

# Combine the RAW data of new notes to the currated training data
commonCols <-  colnames(allNewNotes) %in% colnames(originalNotes) 
newNotes <- allNewNotes[, commonCols]

# for vtreat to work column classes must be consistent, here are examples of how to change

# Numeric Vector
originalNotes$loan_amnt <- as.numeric(originalNotes$loan_amnt)

# Drop "months"
originalNotes$term <- gsub('months', '', originalNotes$term) %>% as.character() %>% as.integer()

# Drop Percents
originalNotes$revol_util <- gsub('%', '', originalNotes$revol_util) %>% as.character() %>% as.numeric()
originalNotes$int_rate <- gsub('%', '', originalNotes$int_rate) %>% as.character() %>% as.numeric()

#Make interger or numeric
originalNotes$revol_bal <- as.numeric(originalNotes$revol_bal) 
originalNotes$pub_rec <- as.integer(originalNotes$pub_rec)

# Change NA to a level "null" which was in training data & make a factor
originalNotes$mths_since_last_major_derog[is.na(originalNotes$mths_since_last_major_derog)] <- 'null'
originalNotes$mths_since_last_major_derog <- as.factor(originalNotes$mths_since_last_major_derog)

## Now easy variable treatment plan
dataPlan <-designTreatmentsC(dframe = originalNotes, 
                             varlist = keeps,
                             outcomename = 'y', 
                             outcometarget = 1)

# Now apply the plan to the training data
treatedOriginal <- prepare(dataPlan, originalNotes)

# Apply to the new notes
treatedNew<- prepare(dataPlan, allNewNotes)

# Now let's do a logistic regression with a 10 fold CV
crtl <- trainControl(method = "cv", 
                     number = 10,
                     verboseIter = TRUE)

# Fit lm model using 10-fold CV: model
finalFit <- train(as.factor(y) ~ ., 
              data = treatedOriginal, #all data once selected algo is identified
              method="glm", family="binomial",
              trControl = crtl)

finalFit

# Consistent Results?
originalProbs <- predict(finalFit, type = 'prob')
cutoffProbs <-ifelse(originalProbs[,2]>=0.8,1,0) #Try 0.9

table(cutoffProbs, originalNotes$y)

# Now on new notes we can actually buy
newProbs <- predict(finalFit, treatedNew, type = 'prob')
table(ifelse(newProbs[,2]>=0.8,1,0))

# Organize data
scoredNotes <- data.frame(id = allNewNotes$id,
                          risk = newProbs[,1],
                          reward = allNewNotes$int_rate,
                          LCgrade = allNewNotes$grade)

# Sort  by least risky and examine
scoredNotes<-scoredNotes[order(scoredNotes$risk),]
head(scoredNotes, 10)

# Visualize
mktPlot <-figure() %>%   ly_points(risk, reward, 
                         data = scoredNotes,
                         color = LCgrade, glyph = LCgrade,
                         hover = list(id, risk, reward, LCgrade))
mktPlot

# Get safest by risk and grade subsections
gradeProfile <- 'A'
riskProfile <- 0.02
subset(scoredNotes, scoredNotes$LCgrade == gradeProfile & scoredNotes$risk <= riskProfile)

# Save artifacts
source('C:/Users/Edward/Desktop/HarvardSummerAdmin2018/Lessons/7_July 25-Finance/day7_scripts/GLMtrimmer_used_Section2.R')
saveRDS(trimTrain(finalFit), 'finalFit.rds')
saveRDS(dataPlan,'finalPlan.rds')

# End
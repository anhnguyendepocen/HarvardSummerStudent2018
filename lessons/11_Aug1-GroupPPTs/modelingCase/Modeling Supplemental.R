#' Author: Ted Kwartler
#' Date: 6-30-2018
#' Purpose: Marketing Modeling Case Supplemental
#' Data from mockaroo.com & https://www.kaggle.com/kondla/carinsurance

# wd
setwd('C:/Users/n0232877/Desktop/CaseStudy/modelingCase/training')

# Libs
library(dplyr)

# Data
trainingDF <- read.csv("CurrentCustomerMktgResults.csv")

newCust <-'C:/Users/n0232877/Desktop/CaseStudy/modelingCase/ProspectiveCustomers.csv'
prospectiveDF <-read.csv(newCust) #need to drop call st and end times

## Perform some EDA and make visuals to understand behavior of accepted vs not accepted offer

## Think about feature enrichment using inner_join(internal, new3rd_party_data) with the 3rd party data

## Perform another bit of EDA with enriched data to see if other insights come out

## There is a form of "data leakage in the internal data that must be removed.  Can you spot it?

## Prepare your modeling data in some manner, you can limit data columns or use all, make changes to type etc.  

## Partition your data set into training, and validation

## Then try a few different algorithms and compare results.
## Here is a first model with non-partitioned, non-enriched & unprepared data (just added NA as a real factor level)...so its not good.
modelingDat <- trainingDF[,c(3:9,12)]
modelingDat$Communication<- addNA(modelingDat$Communication)
modelingDat$Outcome <- addNA(modelingDat$Outcome)
fit <- glm(as.factor(Y_AccetpedOffer) ~., data = modelingDat, family = "binomial")

## Evaluate the model on TRAINING data and provide KPI/ visuals if appropriate
modelingPreds <- predict(fit)

## Evaluate the model on VALIDATION data and provide KPI/ visuals if appropriate


## Compare the KPi differences, usually validation is similar but a bit worse


## Make predictions on new potential customers
preds <- predict(fit, prospectiveDF, type='response')

## Examine the predictions, hist() etc

## Now sort & select the top 10% of prospective customers most likely to respond

# End



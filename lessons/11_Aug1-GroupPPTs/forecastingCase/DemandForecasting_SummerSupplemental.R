#' Author: Ted Kwartler
#' Date: 6-16-2018
#' Purpose: Product Forecasting Case Supplemental

# Libs
library(magrittr)
library(lubridate)
library(dplyr)
library(forecast)

# Options
options(scipen = 999)

# Working Directory
setwd("C:/Users/Edward/Desktop/HarvardSummerAdmin2018/Lessons/11_Aug1-GroupPPTs/forecasting")

# Data
prods <- read.csv('Historical Product Demand.csv')
prods$Order_Demand <- as.numeric(as.character(prods$Order_Demand)) #convert order to number

##### When dealing w/dates it may make sense to engineer vectors, type ?day and ?month to learn more
prods$DateClass <- ymd(prods$Date)

##### I would explore the data with summary() stats & produce some basic plots 
##### to demonstrate data set fluency in your presentation.  
##### You can also subset by prods$Warehouse and perform the EDA
barplot(table(prods$Order_Demand))

# Identify top prods by warehouse
prodTally <- table(prods$Product_Code, prods$Warehouse) %>% as.data.frame()

##### One way to choose the 4 products; WWCP group can choose in other methods
# 4 Top Products
whseTopProds <- prodTally %>% 
  group_by(Var2) %>%
  filter(Freq == max(Freq))

##### For simplicity create 4 separate data sets; here is a way to subset by warehouse and product ID
WhseA <- subset(prods, prods$Warehouse=='Whse_A' & prods$Product_Code=='Product_1496')

##### Put the data in temporal order
WhseA <- WhseA[order(WhseA$DateClass),]
WhseA <- WhseA[complete.cases(WhseA),] #remove NA values

##### Since you are forecasting at the month level, summarize by month, 
##### using the engineered variable $month (you will have to make it similar to line 18)
WhseASummary <- WhseA %>% 
  group_by(yr, month) %>% 
  summarize(amount=sum(Order_Demand))

##### Ugly plot
plot(WhseASummary$amount)
lines(WhseASummary$amount)

##### Quick Time Series Decomposition
Product1496 <- ts(WhseASummary$amount, 
             start = c(2011, 12), 
             end = c(2016, 12), 
             frequency = 12)

##### Review the cylcality; particularly with the tsd1496$seasonal maybe that would be good in the ppt
tsd1496 <- decompose(Product1496)


##### I would then explore at least 1 forecasting method and be able to explain simply in the ppt

##### Don't forget to evaluate the accuracy. Even if your results aren't accurate,
##### it may be helpful to put into the ppt

# End
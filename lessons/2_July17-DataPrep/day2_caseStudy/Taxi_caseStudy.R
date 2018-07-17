#' Author: Ted Kwartler
#' Date: 7-4-2018
#' Purpose: Work on a real and messy data set to perform EDA and get ready for modeling
#' 

# libs
library(fst)
library(lubridate)
library(radiant.data)
library(DataExplorer)

# WD
setwd("C:/Users/Edward/Desktop/HarvardSummerAdmin2018/Lessons/2_July17-DataPrep")

# Data
taxi <-read_fst('yellow_tripdata_2017-12.fst')

# Sample the data for faster EDA; get 5% (divide by 20) of the data
set.seed(1234)
idx <- sample(1:nrow(_____), nrow(_____) %/% __)

sampleTaxi <- _____[_____, ]

# Feature Engineer day of week, duration and hour; be patient will take a minutes
# Change vectors from strings to Date class
sampleTaxi$tpep_pickup_datetime <-as.POSIXct(sampleTaxi$tpep_pickup_datetime)
sampleTaxi$tpep_dropoff_datetime <-as.POSIXct(sampleTaxi$tpep_dropoff_datetime)

# Engineer the Hour as new vectors; add the drop off hour yourself
sampleTaxi$pickUpHour<- hour(sampleTaxi$tpep_pickup_datetime)

# Engineer the day as new vectors; add the drop off day yourself
sampleTaxi$pickUpDay<- day(sampleTaxi$tpep_pickup_datetime)

# Engineer the day of the week as new vectors; add the drop off weekday yourself
sampleTaxi$pickupDayOfWeek <-weekdays(sampleTaxi$tpep_pickup_datetime)

# Trip duration uses difftime(endingTime, startingTime)
sampleTaxi$duration <- difftime(_____, _____, units ='mins')

# Plot relationships
plot(as.numeric(sampleTaxi$duration), sampleTaxi$fare_amount)
cor(as.numeric(sampleTaxi$duration), sampleTaxi$fare_amount)
plot_missing(sampleTaxi)

# How many vendors are in the data set?  
unique(_____)

# How many trips did each take in Jan?
table( _____)

# What is the average trip fare PLUS tip?


# What is the average trip distance?

  
# What is the average number of the trip passengers?

  
# How long (time duration) do trips take on average?


# What day of the week should you work? (requires engineered var)
dayTally <-as.data.frame.matrix(table(sampleTaxi$pickUpDay, sampleTaxi$pickupDayOfWeek))
dayTally
### Can you think of a way to get the average for each column of this df?


# What hours of the day should you work? (requires engineered var)
#This table is called an "arrival pattern" and its view is often used for service agent planning
hrTally <-as.data.frame.matrix(table(_____, _______$pickupDayOfWeek))
### Can you think of a way to "apply" the function which.max to each vector at once?

# Should you work on holidays? (Hanukkah was Dec12-Dec20, Christmas Dec24-25 & Kwanzaa Dec 26-Jan1, last date not in data)
barplot(table(sampleTaxi$pickUpDay))

# What other insights can you find when using radiant.data?
write.csv(______, 'sampleTaxi.csv',row.names=F)
write.csv(______, 'sampleTaxi.csv',row.names=F)
radiant.data::radiant.data_window()

# End

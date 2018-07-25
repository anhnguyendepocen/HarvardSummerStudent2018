#' Author: Ted Kwartler
#' Date: 6-16-2018
#' Purpose: Modeling Risk in MTG
#' 

# Install the boosterbox package
#install.packages('triangle')
#install.packages("~/HarvardSummerAdmin2018/Lessons/7_July 25/BoosterBox_0.0.2.tar.gz", 
#                 repos = NULL, type = "source")

# Library
library(BoosterBox)

# Example data formatting
data(modernMasters13)
head(modernMasters13)

# Get a set's prices; the site restricts webscraping so you may have to try 3 or 4 times to get the information without an error.  You can also supply a data frame to other functions if the data doesn't download.
#  http://magic.tcgplayer.com/db/search_result.asp?Set_Name=Iconic%20Masters
iconic<-getTCGprices("Iconic Masters")

# If that fails, copy/paste from th TCG site and save a CSV version.
# Data Integrity! High for  Genesis Wave was corrected in CSV
iconic <- read.csv('C:/Users/Edward/Desktop/HarvardSummerAdmin2018/Lessons/7_July 25-Finance/day7_data/MTG/IconicMasters.csv')

# Examine
head(iconic)

# sometimes weird encoding when webscraping needs to be cleaned up
iconic$Name<-gsub('Â', '', iconic$Name)
iconic$Set<-gsub('Â', '', iconic$Set)
head(iconic)

# Single pack simulation; pacsPerFoil is usually 6 but Iconic Masters is a premium set
crackPack(iconic, packsPerFoil = 1)

# Get another pack
(onePack <- crackPack(iconic,packsPerFoil = 1))

# Simulate market valuation at the pack level
cardValues(onePack, worthlessCommons = T, verbose = F)

# Simulate market valuation at the card level
(cardVals <- cardValues(onePack, worthlessCommons = T, verbose = T))
sum(cardVals$TCGdistPrice)

# Let's open a box of booster packs; 24 for Iconic Masters but usually 36 for other sets
simBox <- openBox(iconic, 
                  numPacks = 24, 
                  packsPerMythic = 8, 
                  packsPerFoil = 1,
                  foilsInSet = T)

# What is the expected return for a complete booster box?
(boxReturn <- cardValues(simBox))


## Now let's simulate opening 1000 packs to feel more confident in the results
packsSim <- ls()
for (i in 1:1000){
  cards <- crackPack(iconic, packsPerFoil = 1)
  cards <- cardValues(cards)
  nam <- i
  print(paste('opening pack',i))
  packsSim[[nam]] <- cards
}

# Avg pack return (expected value); Note this number!
mean(as.numeric(unlist(packsSim)))

## What about opening 100 boxes (2400 packs)
boxSim <-list()
for (i in 1:100){
  boxes <- openBox(iconic, numPacks = 24, packsPerMythic = 8, 
                    packsPerFoil = 1, foilsInSet = T)
  boxes <- cardValues(boxes, verbose = T)
  nam <- i
  boxes$boxNum <-i
  print(paste('opening box',i))
  boxSim[[nam]] <- boxes
}

# Organize the verbose outcome
boxSim <- do.call(rbind, boxSim)

# See what verbose=T does
boxSim[1:10,]

# Avg box return
indBoxes <- aggregate(boxSim$TCGdistPrice, by = list(boxSim$boxNum), FUN = sum)
head(indBoxes)
(boxAVG <- mean(indBoxes[,2]))

# Plot and Review 
boxPrice <- 129.00
hist(unlist(indBoxes[,2]), main='Iconic Masters')
abline(v=boxPrice,col="red")
text(boxPrice,10,'cost', col='red', pos=1,srt=90, cex=1)
abline(v=boxAVG,col="blue")
text(boxAVG,10,'AvgReturn',col='blue', pos=1,srt=90, cex=1)

# number of boxes below cost
length(subset(indBoxes[,2],indBoxes[,2]<=boxPrice))


######## Find a card value
idx <-grep('Genesis Wave', iconic$Name)
iconic[idx,]

# https://goo.gl/SmhXYv
# https://docs.google.com/spreadsheets/d/1D8bk5n5riD-bVmmEUDSDg7qqcNptBwGJjekiw0C1rBY/edit?usp=sharing

# End


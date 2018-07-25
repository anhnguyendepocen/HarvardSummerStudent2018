#' Author: Ted Kwartler
#' Date: 6-18-2018
#' Purpose: Lending Club Model Scoring as a standalone function
#'

# options
options(scipen = 999)

# Cleaner Version Function
scoreNew <- function(notesPth, planPath, fitPath, gradeProfile, riskProfile){
  # Libs
  require(caret)
  require(e1071)
  require(vtreat)
  require(dplyr)
  require(rbokeh)
  
  # Objs
  dataPlan <- readRDS(planPath)
  fit <- readRDS(fitPath)
  newNotes <- read.csv(notesPth)

  # Prep Data
  allNewNotes<- prepare(dataPlan, newNotes)
  
  # Score & Organize
  newProbs <- predict(fit, allNewNotes, type = 'prob')
  scoredNotes <- data.frame(id = newNotes$id,
                            risk = newProbs[,1],
                            reward = newNotes$int_rate,
                            LCgrade = newNotes$grade)
  scoredNotes<-scoredNotes[order(scoredNotes$risk),]
  
  # Viz
  mktPlot <- figure() %>%   ly_points(risk, reward, 
                           data = scoredNotes,
                           color = LCgrade, glyph = LCgrade,
                           hover = list(id, risk, reward, LCgrade))
  # Response
  res <-subset(scoredNotes, scoredNotes$LCgrade == gradeProfile & scoredNotes$risk <= riskProfile)
  res<- list(res = res, mktPlot = mktPlot)
  return(res)
}

# Score Inputs
notes <-'C:/Users/Edward/Desktop/HarvardSummerAdmin2018/Lessons/7_July 25-Finance/day7_data/LC/primaryMarketNotes_browseNotes_1-RETAIL-June24.csv'
plan <- 'C:/Users/Edward/Desktop/HarvardSummerAdmin2018/Lessons/7_July 25-Finance/day7_data/LC/finalPlan.rds'
model <- 'C:/Users/Edward/Desktop/HarvardSummerAdmin2018/Lessons/7_July 25-Finance/day7_data/LC/finalFit.rds'

scoreNew(notesPth = notes,
         planPath = plan, 
         fitPath = model, 
         gradeProfile = 'A',
         riskProfile = 0.02)
# End

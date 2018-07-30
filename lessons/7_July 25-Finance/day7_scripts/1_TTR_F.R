#' Author: Ted Kwartler
#' Date: 6-22-2018
#' Purpose: RSI Example
#' 

# Opts
options(scipen=999)

# Libs
library(TTR)
library(quantmod)
library(PerformanceAnalytics)
library(dygraphs)
library(htmltools)

# Get Coke
getSymbols("MSFT")
AMZN <- MSFT['2015-01-01/']

# Calc RSI
AMZNrsi <-RSI(AMZN$MSFT.Close,
    maType="SMA", #Usually EMA; not covered
    n =14)

summary(AMZNrsi)

# Visualize 
browsable(
  tagList(
    dygraph(AMZN$MSFT.Close, group = "Price", height = 200, width = "100%"),
    dygraph(AMZNrsi, group = "Price", height = 200, width = "100%") %>%
      dyLimit(30, label = 'OverSold') %>%
      dyLimit(70, label = 'OverBought') %>%
      dyRangeSelector()
  )
)

# End
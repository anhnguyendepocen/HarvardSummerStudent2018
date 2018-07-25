#' Author: Ted Kwartler
#' Date: 6-22-2018
#' Purpose: MACD Example As Indicator for ETH
#' 

# Opts
options(scipen=999)

# Libs
library(TTR)
library(quantmod)
library(PerformanceAnalytics)
library(dygraphs)
library(htmltools)
library(jsonlite)

# Get historical to Sys.Date() USD to ETH data
ago<-(Sys.Date())%>% as.POSIXct() %>% as.numeric()

# API URL
eth <- paste0('https://poloniex.com/public?command=returnChartData&currencyPair=USDT_ETH&start=1407801600&end=',ago,'&period=86400')

# GET request 
ethDF <- fromJSON(eth)

# Examine
head(ethDF)

# Real Date
ethDF$ezDate <- as.POSIXct(ethDF$date, origin="1970-01-01")

# Make into XTS
ethDF <- xts(ethDF$close, order.by = ethDF$ezDate )
ethDF <- ethDF['2017-06-22/2018-06-22']

# MACD Indicator
ETHmacd <- MACD(ethDF,
                nFast = 12, nSlow = 26, nSig = 9, 
                maType="SMA", #Usually EMA; not covered
                percent = F) # Values or Percents

# Trade In/Out
tradeSignal <- Lag(ifelse(ETHmacd[,1] > ETHmacd[,2]  , 1, 0))
ret <- ROC(ethDF) * tradeSignal

# How did ETH do in this timeframe?
plot(ethDF)

# Review the return
charts.PerformanceSummary(ret)  


# Sanity check with visual
# Now let's visualize in a stacked dynamic plot
browsable(
  tagList(
    dygraph(ethDF, group = "Price", height = 200, width = "100%"),
    dygraph(ETHmacd,group = "Price", height = 200, width = "100%") %>%
      dySeries('macd',label='MACD') %>%
      dySeries('signal',label='SIGNAL') %>%
      dyRangeSelector()
  )
)

# End
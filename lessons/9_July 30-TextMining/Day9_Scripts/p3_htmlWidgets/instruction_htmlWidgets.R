#' Title: HTML Widgets
#' Purpose: Learn about html widgets 
#' Author: Ted Kwartler
#' email: ehk116@gmail.com
#' License: GPL>=3
#' Date: 2018-4-24
#' 

# Set the working directory
setwd("~/HarvardSummerAdmin2018/Lessons/July 30/Day9_Data")

# Get up to date wrodcloud2 w/o bugs
#library(devtools)
#devtools::install_github("lchiffon/wordcloud2")

# Libs
# Need to restart R sometimes due to wc2 function rending
library(tm)
library(wordcloud2)
library(rbokeh)
library(radarchart)
library(stringi)
library(stringr)

# Options & Functions
tryTolower <- function(x){
  y = NA
  try_error = tryCatch(tolower(x), error = function(e) e)
  if (!inherits(try_error, 'error'))
    y = tolower(x)
  return(y)
}

cleanCorpus<-function(corpus){
  corpus <- tm_map(corpus, content_transformer(tryTolower))
  corpus <- tm_map(corpus, removeWords, stopwords('SMART')) # FYI different than training
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, removeNumbers)
  return(corpus)
}

# Get text
#Windows needs explicit encode
txt<-read.csv('news.csv', stringsAsFactors = F, encoding='UTF-8') 

# Corpus Preprocessing & Organization
txtCorpus <- VCorpus(DataframeSource(txt))
txtCorpus<-cleanCorpus(txtCorpus)

# Make DTM
txtDTM<-DocumentTermMatrix(txtCorpus)

# Make a simple matrix version
txtM<-as.matrix(txtDTM)

###
## Begin HTMLwidgets
# JS Wordcloud
dtmVec <- sort(colSums(txtM),decreasing=TRUE)
wcDF <- data.frame(word = names(dtmVec),freq=dtmVec)
wordcloud2(wcDF[1:200,], size = .5)

# Most freq word associations
topTerm<-as.character(wcDF[1,1])
associations<-findAssocs(txtDTM, topTerm, 0.30)
assocDF<-data.frame(term=names(associations[[1]]),value=unlist(associations))

## Bokeh Barplot
figure() %>%
  ly_bar(x=term, y=value,data=assocDF ,hover = TRUE) %>%
  x_axis(label='term') %>%
  y_axis(label=paste(topTerm,'association value')) %>%
  theme_axis("x", major_label_orientation = 90) 


## Top Term Density
topTerms<-as.character(wcDF[1:8,1])
topTerms<- paste(topTerms, collapse='|')
txt$top_term_density<-stri_count_regex(txt$text, pattern=topTerms)
topDensity<-aggregate(top_term_density ~ doc_id, txt, sum)
topDensity<-topDensity[order(topDensity$top_term_density, decreasing=T),]
topDensity<-subset(topDensity,topDensity$top_term_density>1) #reduce to 2+ mentions

## Make radarchart
tops<-subset(topDensity,topDensity$top_term_density>0)
chartJSRadar(scores=tops, main=topTerms)

## Scatter Plot
# count avg word length in article description
txt$strCount<-str_count(txt$text, pattern = ' ')
avgLength<-aggregate(strCount ~ doc_id, txt, mean)

# Count unique words in article description 
uniqueStr<-str_split(txt$text, pattern = " ")
uniqueStr<-lapply(uniqueStr,unique)
txt$strUnique<-unlist(lapply(uniqueStr,length))
avgDiversity<-aggregate(strUnique ~ doc_id, txt, mean)

# Count number of text records by source
numArticles<-aggregate(text ~ doc_id, txt, length)

# Merge accounts for missing
scatterDF<-merge(avgLength,avgDiversity, all=T)
scatterDF<-merge(scatterDF,numArticles, all=T)

# Make scatter chart
s<-figure() %>%
  ly_points(x=strCount, y=strUnique, data = scatterDF,
            hover = list(doc_id,strCount, strUnique, text)) %>%
  x_axis(label='Total Word Count') %>%
  y_axis(label='Word Diversity')

s

# Save a widget with
#htmlwidgets::saveWidget(s, file="scatter_chart.html")

# End

#' Title: Get Text : API
#' Purpose: Grab some text from an API for instruction 
#' Author: Ted Kwartler
#' email: ehk116@gmail.com
#' License: GPL>=3
#' Date: 2018-4-24
#' 

# Load Libraries
library(jsonlite)
library(pbapply)

# Options
options(stringsAsFactors = F)

# www.newsapi.org Key
news<-'c6166540623e4588b2b8b53143c1299a'

# Examine a single API endpoint
url<-paste0('https://newsapi.org/v1/articles?source=techcrunch&apiKey=',news) 
url

# Endpoint for all news sources
newsSources<-fromJSON('https://newsapi.org/v1/sources?language=en')

# Examine the response
str(newsSources[[1]])
str(newsSources[[2]])

# Example Loop
for (i in 1:length(newsSources$sources$id)){
  print('found news source')
  print(newsSources$sources$id[i])
}

# Url construction
newsUrls<-vector()  
for (i in 1:length(newsSources$sources$id)){
  x<-paste0('https://newsapi.org/v1/articles?source=',
         newsSources$sources$id[i],
         '&apiKey=',news)
  naming<-newsSources$sources$name[i]
  newsUrls[naming]<-x
}


# Examine some of the constructed urls
newsUrls[23]
newsUrls[47]

# GET request from each endpoint & examine 
allNews<-pblapply(newsUrls,fromJSON)
str(allNews[[1]])

# Save a copy just in case
# saveRDS(allNews,'~/ODSC/workshop_data/allNews.rds')
# allNews<-readRDS('~/ODSC/workshop_data/allNews.rds')

# Another loop to append source to EACH article & examine
allDescriptions<-list()
for (i in 1:length(allNews)){
  x<-as.data.frame(allNews[[i]][4])
  x$source<-unlist(allNews[[i]][2])
  nam<-i
  allDescriptions[[i]]<-x
}

str(allDescriptions[[1]])

# Organize into a single df
newsDescriptions<-do.call(rbind,allDescriptions)
names(newsDescriptions)

# Organize all the text & examine
#allNews<-do.call(rbind,allNews)
txt<-data.frame(doc_id=newsDescriptions$source, text=newsDescriptions$articles.description)
txt[1,]

# Save a copy
write.csv(txt,'~/news.csv', row.names=F)

# End

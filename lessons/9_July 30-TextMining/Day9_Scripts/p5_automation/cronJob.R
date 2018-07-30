#' Title: Automation : Send Dashboard 
#' Purpose: Using an email API service, send the dashboard daily 
#' Author: Ted Kwartler
#' email: ehk116@gmail.com
#' License: GPL>=3
#' Date: 2018-4-24
#' 

# Load Libraries
library(rmarkdown)
library(httr)
library(jsonlite)
library(pbapply)

# Set WD
setwd('~/ODSC/p5_automation')

# Params; uncomment when in prod
#emailKey<-'xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx' #https://elasticemail.com/ Key
#newsKey<-'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx' # www.newsapi.org Key
#emails<-paste('xxxxxx@gmail.com','xxxxxxxxxx@yahoo.com', sep=', ')

#Time
st<-Sys.time()

### Get News API
# Options
options(stringsAsFactors = F)

# Endpoint for all news sources
newsSources<-fromJSON('https://newsapi.org/v1/sources?language=en')

# Url construction
newsUrls<-vector()  
for (i in 1:length(newsSources$sources$id)){
  x<-paste0('https://newsapi.org/v1/articles?source=',
            newsSources$sources$id[i],
            '&apiKey=',newsKey)
  naming<-newsSources$sources$name[i]
  newsUrls[naming]<-x
}

# GET request from each endpoint & examine 
allNews<-pblapply(newsUrls,fromJSON)

# Another loop to append source to EACH article
allDescriptions<-list()
for (i in 1:length(allNews)){
  x<-as.data.frame(allNews[[i]][4])
  x$source<-unlist(allNews[[i]][2])
  nam<-i
  allDescriptions[[i]]<-x
}

# Organize into a single df
newsDescriptions<-do.call(rbind,allDescriptions)

txt<-data.frame(doc_id=newsDescriptions$source, text=newsDescriptions$articles.description)

# Save a copy
write.csv(txt,'news.csv', row.names=F)

###

# Render Markdown; will save in working directory
render("cronDash.Rmd")

# Time
end<-Sys.time()

# Send Email w/Attachment
tmp<-list.files(pattern='*.html',full.names=T)
msg<-paste(Sys.Date(),'\ncronJob.R Success\n','Completed in: \n',end-st,'\n\n')
baseURL<-paste0('http://api.elasticemail.com/v2/email/send?apikey=',emailKey)
r<-POST(baseURL,body=list(
  bodyText=msg,
  to=emails,
  subject='cronJob.R Success',
  isTransactional='TRUE',
  attachment=upload_file(tmp),
  from='ehk116@gmail.com'))

# Print outcome to log
print(parsed_content(r)[1])

# End
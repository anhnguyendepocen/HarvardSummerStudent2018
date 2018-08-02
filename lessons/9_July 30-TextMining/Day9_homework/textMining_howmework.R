#' Title: Text Mining Homework
#' Purpose: Identify what reviews associate with good and bad reviews
#' Author: Ted Kwartler
#' email: ehk116@gmail.com
#' License: GPL>=3
#' Date: 2018-4-24
#' Instructions:
#' Fill in the rest of the script, and answer the questions inline.  Then upload to canvas.

# Set the working directory
setwd("~/HarvardSummerStudent2018/lessons/9_July 30-TextMining/Day9_homework")

# Libs
library(tm) #tm package 
library(qdap)
library(wordcloud) # package for wordclouds
library(RColorBrewer)

# Options & Functions
options(stringsAsFactors = FALSE)
Sys.setlocale('LC_ALL','C')

tryTolower <- function(x){
  y = NA
  try_error = tryCatch(tolower(x), error = function(e) e)
  if (!inherits(try_error, 'error'))
    y = tolower(x)
  return(y)
}

cleanCorpus<-function(corpus){
  corpus <- tm_map(corpus, content_transformer(qdapRegex::rm_url))
  print('removed urls')
  corpus <- tm_map(corpus, content_transformer(replace_contraction)) #new: isn't to is not
  print('replace contractions')
  corpus <- tm_map(corpus, content_transformer(replace_symbol)) #new: @ to "at"
  print('replace symbol')
  corpus <- tm_map(corpus, removePunctuation)
  print('remove punctuation')
  corpus <- tm_map(corpus, stripWhitespace)
  print('stip whitespace')
  corpus <- tm_map(corpus, removeNumbers)
  print('remove numbers')
  corpus <- tm_map(corpus, content_transformer(tryTolower))
  print('making lowercase')
  corpus <- tm_map(corpus, removeWords, customStopwords)
  print('DONE!')
  return(corpus)
}

# Get your data
text <- read.csv('sampledAirBnB_Boston.csv')

# 1. How many comments are there BEFORE the sample?
# Answer: 4312

# Get a sample of 1000 to speed up time
set.seed(1234)
idx <- sample (1:nrow(text),1000)
text <- text[idx, ]

# Examine
dim(text)
text$comments[1]

# Define custom stopwords, add "apartment" and "boston" to the list
customStopwords <- c(stopwords('english'), 'place', 'apartment', 'boston')

# Make the VECTOR of text called $comments into a volatile corpus 
txtCorpus <- VCorpus(VectorSource(text$comments))

# Apply the cleaning function to the volatile corpus; takes a moment!!
txtCorpus<-cleanCorpus(txtCorpus)

# Make a Document Term Matrix 
txtDTM <- DocumentTermMatrix(txtCorpus)

# 2. How many terms are in this DTM?
# Answer: 5690 //FIXME fix qdap

# Convert TDM to a simple matrix; takes a moment
txtDTMm <- as.matrix(txtDTM)

# Get column Sums and sort decreasing =TRUE
txtDTMv <- sort(colSums(txtDTMm),decreasing=TRUE)

# Organize the row sums
txtDF <- data.frame(word = names(txtDTMv),freq=txtDTMv)

# Examine the first 6 rows
head(txtDF)

# 3. What is the third most frequent term used in the reviews?
# Answer: clean

# Choose the "Purples" color & drop light ones in the color palette
pal <- brewer.pal(8, "Purples")
pal <- pal[-(1:2)]

# Make a simple word cloud of all terms with 75 words with your palette object
set.seed(1234)
wordcloud(txtDF$word,txtDF$freq, max.words=75, random.order=FALSE, colors=pal)

# 4. Overall, are comments using positive or negative terms frequently?
# Answer: positive

# Now that you have examined the entire corpus, time to split it based on polarity(); this take a while!
polarityScores <- polarity(text$comments)

# 5. What is the average polarity score for the comments?
# Answer: 0.883

# Using each document's polarity score segregate the comments into positive and negative corpora
justScores <- polarityScores$all$polarity # extract just the polarity values
documentClass <- ifelse(justScores>0,TRUE,FALSE) # change to binary
documentClass[is.na(documentClass)] <- 0 # clean up any NA values
table(documentClass)

# 6. What is the number of positive reviews versus negative reviews?
# Answer: 70 negative, 930 positive

# Now you can subset the original documents by their positive or negative nature and transpose them to TDM
documentClass <- which(documentClass == 1)
posReviews <- text$comments[documentClass]
negReviews <- text$comments[-documentClass]

# Collapse them into a single corpus for each class
posReviews <- paste(posReviews, collapse = ' ')
negReviews <- paste(negReviews, collapse = ' ')

# Declare each as a volatile corpus from a vector source
posReviews <- VCorpus(VectorSource(posReviews))
negReviews <- VCorpus(VectorSource(negReviews))

# Clean each of the corpora; takes awhile
posReviews<-cleanCorpus(posReviews)
negReviews<-cleanCorpus(negReviews)

# Collapse the cleaned segregated corpora
posReviews <- paste(posReviews, collapse = " ")
negReviews <- paste(negReviews, collapse = " ")

# Make a combined corpus of pos/neg
allReviews <- c(posReviews, negReviews)

# Now combine into a VCorpus, using a VectorSource
allReviewsCorpus <- VCorpus(VectorSource(allReviews))

# 7. How many documents are in this corpus?
# Answer: 2

# Make TDM and change to a simple matrix
allReviewsTDM <- TermDocumentMatrix(allReviewsCorpus)
allReviewsTDMm <- as.matrix(allReviewsTDM)

# Label the new TDM, remember the order of subjects
colnames(allReviewsTDMm) = c("Positive", "Negative")

# Make comparison cloud with 75 words, random.order = FALSE, and title.size = 0.5
set.seed(1234)
comparison.cloud(allReviewsTDMm, max.words=75, random.order=FALSE,
                 title.size=0.5,colors=brewer.pal(ncol(allReviewsTDMm),"Dark2"))

# 8. Based on this visual (remember its only a sample so may not be accurate).  
# What are the 4 most frequent terms used in positive reviews?  Does this indicate a host matters?
# Answer: "great", "clean", "stay", "nice".
#         "host" appears as a very small word on the right, so it doesn't seem to matter

# End

#Installing Libraries
# install.packages("dplyr")
# install.packages("tm")
# install.packages("stringr")
# install.packages("RColorBrewer")
# install.packages("wordcloud")
# install.packages("topicmodels")
# install.packages("ggplot2")
# install.packages("LDAvis")
# install.packages("servr")
# install.packages("SnowballC")
# install.packages("textcat")
# install.packages("textmineR")
# install.packages("tidyverse")
# install.packages("textstem")

library(dplyr) # basic data manipulation 
library(tm) # package for text mining package 
library(stringr) # package for dealing with string
library(RColorBrewer)# package to get special theme color 
library(wordcloud) # package to create wordcloud 
library(ggplot2) # basic data visualization 
library(SnowballC) #UTF-8 Library
library(textmineR) #Text mining library
library(tidyverse)   #To tidy the data structure
library(textstem) # For stemming and lemmatization

rm(list=ls())

#Setting the location of the file
getwd()
setwd("/Users/epk/Desktop/R_Csv")

#Reading the Google Data
google_data <- read.csv("google_data.csv",stringsAsFactors = TRUE )

#Conversion of Review Text to standard form, some outlier texts can't read for tm_map
google_keyword <- stringr::str_conv(google_data$Search.keyword, "UTF-8")
google_description <- stringr::str_conv(google_data$Description, "UTF-8")

# Create Corpus
docs_google_keyword <- Corpus(VectorSource(google_keyword))
docs_google_description <- Corpus(VectorSource(google_description))

#Lemmatisation of Data
docs_google_keyword <- tm_map(docs_google_keyword, lemmatize_strings)
docs_google_description <- tm_map(docs_google_description, lemmatize_strings)

#Creation of Document Term Matrix 
dtmdocs_google_keyword <- DocumentTermMatrix(docs_google_keyword, 
                                        control = list(tolower =TRUE,lemma=TRUE, removePunctuation = TRUE,
                                                       removeNumbers = TRUE, stopwords = TRUE,
                                                       stripWhitespace = TRUE))
dtmocs_google_description <- DocumentTermMatrix(docs_google_description, 
                                             control = list(tolower =TRUE,lemma=TRUE, removePunctuation = TRUE,
                                                            removeNumbers = TRUE, stopwords = TRUE,
                                                            stripWhitespace = TRUE))

#Applying  sum function to obtain the  frequency of words in each text.
raw.sum_keyword<- apply(dtmdocs_google_keyword,1,FUN=sum) 
raw.sum_description <- apply(dtmocs_google_description,1,FUN=sum)


#Storing to the document with the frequency of words ! = 0 
dtmdocs_google_keyword <- dtmdocs_google_keyword[raw.sum_keyword!=0,]
dtmocs_google_description <- dtmocs_google_description[raw.sum_description!=0,]


#Converting to Matrix 
dtmdocs_google_keyword_update <- as.matrix(dtmdocs_google_keyword)
dtmocs_google_descriptionupdate <- as.matrix(dtmocs_google_description)


#Obtaining Word Frequency
frequency_keyword <- colSums(dtmdocs_google_keyword_update)
frequency_description <- colSums(dtmocs_google_descriptionupdate)

#Sorting the words 
frequency_keyword <- sort(frequency_keyword, decreasing=TRUE) 
frequency_description <- sort(frequency_description, decreasing=TRUE) 


#Total Length of doc
doc_length_keyword  <- rowSums(dtmdocs_google_keyword_update)
doc_length_description <- rowSums(dtmocs_google_descriptionupdate)


#Example of the output
frequency_keyword[1:10] 
frequency_description[1:10] 


# get back the word
words_keyword <- names(frequency_keyword)
words_description <- names(frequency_description)


#Displaying the most frequent words in while searchig in keywords

layout(matrix(c(1, 2), nrow=2), heights=c(1, 4))
par(mar=rep(0, 4))
plot.new()
text(x=0.5, y=0.5, "Word Cloud of Most Frequent Keywords Searched in Google",col="blue")


wordcloud(words_keyword[1:50], frequency_keyword[1:50], rot.per=0.15, random.order = FALSE, scale=c(4,0.5),
          random.color = FALSE, colors=brewer.pal(8,"Dark2"))

#Displaying the most frequent words in Description of Google Ad

layout(matrix(c(1, 2), nrow=2), heights=c(1, 4))
par(mar=rep(0, 4))
plot.new()
text(x=0.5, y=0.5, "Word Cloud of most frequent words in Description of Google Ad",col="blue")
wordcloud(words_description[1:100], frequency_description[1:100], rot.per=0.15, random.order = FALSE, scale=c(4,0.5),
          random.color = FALSE, colors=brewer.pal(8,"Set2"))


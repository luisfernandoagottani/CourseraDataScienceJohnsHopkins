## CAPSTONE PROJECT WEEK 1

### TASK 0 : UNDERSTANDING THE PROBLEM

setwd("C:/Users/Luis/Documents/Capstone Project")

library(tm)

filename<- "CapstoneDataSet.zip"

## File exists? -No - (Download)
if (!file.exists(filename)){
  fileURL <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
  download.file(fileURL, filename)
}

## Folder UCI HAR Dataset exists? - No - (Download)
  unzip(filename)

## QUIZ WEEK 1
  
### Q1
setwd("C:/Users/Luis/Documents/Capstone Project/final/en_US")

en_blogs<- readLines("en_US.blogs.txt")

FileInfo <- file.info("en_US.blogs.txt")
sizeB <- FileInfo$size
sizeB

### Q2
setwd("C:/Users/Luis/Documents/Capstone Project/final/en_US")

en_twitter<- readLines("en_US.twitter.txt")

summary(en_twitter)

### Q3
setwd("C:/Users/Luis/Documents/Capstone Project/final/en_US")

en_twitter<- readLines("en_US.twitter.txt")
en_blogs<- readLines("en_US.blogs.txt")
en_news<- readLines("en_US.news.txt")

max(nchar(en_news))
max(nchar(en_twiter))
max(nchar(en_blogs))

biggest<- structure(list(news = max(nchar(en_news)),
                         twitter = max(nchar(en_twitter)),
                         blogs = max(nchar(en_blogs))),
                    .Names = c("news", "twitter", "blogs"),
                    row.names = c(NA, 1L), class = "data.frame")
### Q4

setwd("C:/Users/Luis/Documents/Capstone Project/final/en_US")

en_twitter<- readLines("en_US.twitter.txt")

lovelines <- (grepl(" love ", en_twitter))
numlove <- table(lovelines)["TRUE"]

hatelines <- (grepl(" hate ", en_twitter))
numhate <- table(hatelines)["TRUE"]

proportion<- numlove/numhate

# Q5
setwd("C:/Users/Luis/Documents/Capstone Project/final/en_US")
twitterLines <- readLines("en_US.twitter.txt")
lineTarget <- grep("biostats",twitterLines)
twitterLines[lineTarget]

# Q6
setwd("C:/Users/Luis/Documents/Capstone Project/final/en_US")
phrase <- (grepl("A computer once beat me at chess, but it was no match for me at kickboxing", en_twitter))
numphrase <- table(phrase)["TRUE"]
numphrase

### Week 2

# Task 2
###FIRST MODEL
setwd("C:/Users/Luis/Documents/Capstone Project/final/en_US")

en_twitter <- readLines("en_US.twitter.txt")
Sen_twitter <- scan("en_US.twitter.txt","character",sep="\n")

#Replace full stop and comma
Sen_twitter<-gsub("\\.","",Sen_twitter)
Sen_twitter<-gsub("\\,","",Sen_twitter)

#Split sentence
Wen_twitter<-strsplit(Sen_twitter," ")

#Calculate word frequencies
WFen_twitter<-table(unlist(Wen_twitter))

#DataFrame
library(dplyr)
wordfrequency1<- cbind.data.frame(word=names(WFen_twitter),qtde=as.integer(WFen_twitter))
wordfrequency1<- arrange(wordfrequency1,desc(qtde))
###SECOND MODEL
setwd("C:/Users/Luis/Documents/Capstone Project/final/en_US")

en_twitter <- readLines("en_US.twitter.txt")

docs <- Corpus(VectorSource(en_twitter))

toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")

# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))
# Remove numbers
docs <- tm_map(docs, removeNumbers)
# Remove english common stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs <- tm_map(docs, removeWords, c("blabla1", "blabla2")) 
# Remove punctuations
docs <- tm_map(docs, removePunctuation)
# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)
# Text stemming
# docs <- tm_map(docs, stemDocument)

dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)

#THIRD MODEL
setwd("C:/Users/Luis/Documents/Capstone Project/final/en_US")

en_twitter <- readLines("en_US.twitter.txt")

corpus<-Corpus(VectorSource(en_twitter))
corpus<-tm_map(corpus, content_transformer(tolower))
corpus<-tm_map(corpus, removeNumbers)
corpus<-tm_map(corpus, removeWords, stopwords('english'))
#corpus<-tm_map(corpus, stemDocument, language = "english") 
corpus<-tm_map(corpus, removePunctuation)

tdm<-TermDocumentMatrix(corpus)

tdmatrix<-as.matrix(tdm)
wordfreq<-sort(rowSums(tdmatrix), decreasing = TRUE)

##WEEK 3

library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library("ggplot2")
library("dplyr")
library("tidyr")
library("reshape2")
library("reshape")
library("RWeka")

## Download Doc

setwd("C:/Users/Luis/Documents/Capstone Project")
filename<- "CapstoneDataSet.zip"

if (!file.exists(filename)){
  fileURL <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
  download.file(fileURL, filename)
  unzip(filename)
}

## Reading Doc

setwd("C:/Users/Luis/Documents/Capstone Project/final/en_US")

en_blogs<- readLines("en_US.blogs.txt", encoding = 'UTF-8')
en_twitter<- readLines("en_US.twitter.txt", encoding = 'UTF-8')
en_news<- readLines("en_US.news.txt", encoding = 'UTF-8')

## Training data

set.seed(2019)

sm_blogs<-en_blogs[rbinom(length(en_blogs)*.01, length(en_blogs), .5)]
write.csv(sm_blogs, file = "C:/Users/Luis/Documents/Capstone Project/final/en_US_Samples/sm_blogs.csv", row.names = FALSE)

sm_twitter<-en_twitter[rbinom(length(en_twitter)*.01, length(en_twitter), .5)]
write.csv(sm_twitter, file = "C:/Users/Luis/Documents/Capstone Project/final/en_US_Samples/sm_twitter.csv", row.names = FALSE)

sm_news<-en_news[rbinom(length(en_news)*.01, length(en_news), .5)]
write.csv(sm_news, file = "C:/Users/Luis/Documents/Capstone Project/final/en_US_Samples/sm_news.csv", row.names = FALSE)

samplefiles <- file.path("C:/Users/Luis/Documents/Capstone Project/final", "en_US_Samples") 

docs <- Corpus(DirSource(samplefiles))
docs <- VCorpus(VectorSource(docs))

## Cleaning Data

setwd("C:/Users/Luis/Documents/Capstone Project/final/en_US_Samples")
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "-")
docs <- tm_map(docs, toSpace, ":")
docs <- tm_map(docs, toSpace, "'")
docs <- tm_map(docs, toSpace, "'")
docs <- tm_map(docs, toSpace, " -")
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, stripWhitespace)
docs <- tm_map(docs, PlainTextDocument)

## n-grams

unigram<-function(x) NGramTokenizer(x,Weka_control(min=1,max=1))
unigramtab<-TermDocumentMatrix(docs,control=list(tokenize=unigram))
unigramcorpus<-findFreqTerms(unigramtab,lowfreq=1000)
unigramcorpusnum<-rowSums(as.matrix(unigramtab[unigramcorpus,]))
unigramcorpustab<-data.frame(Word=names(unigramcorpusnum),frequency=unigramcorpusnum)
unigramcorpussort<-unigramcorpustab[order(-unigramcorpustab$frequency),]

bigram<-function(x) NGramTokenizer(x,Weka_control(min=2,max=2))
bigramtab<-TermDocumentMatrix(docs,control=list(tokenize=bigram))
bigramcorpus<-findFreqTerms(bigramtab,lowfreq=1)
bigramcorpusnum<-rowSums(as.matrix(bigramtab[bigramcorpus,]))
bigramcorpustab<-data.frame(Word=names(bigramcorpusnum),frequency=bigramcorpusnum)
bigramcorpussort<-bigramcorpustab[order(-bigramcorpustab$frequency),]
bigramfinal<- separate(bigramcorpussort, Word,c("word1", "word2"), sep=" ")

trigram<-function(x) NGramTokenizer(x,Weka_control(min=3,max=3))
trigramtab<-TermDocumentMatrix(docs,control=list(tokenize=trigram))
trigramcorpus<-findFreqTerms(trigramtab,lowfreq=1)
trigramcorpusnum<-rowSums(as.matrix(trigramtab[trigramcorpus,]))
trigramcorpustab<-data.frame(Word=names(trigramcorpusnum),frequency=trigramcorpusnum)
trigramcorpussort<-trigramcorpustab[order(-trigramcorpustab$frequency),]
trigramfinal<- separate(trigramcorpussort, Word,c("word1", "word2", "word3"), sep=" ")

tetragram<-function(x) NGramTokenizer(x,Weka_control(min=4,max=4))
tetragramtab<-TermDocumentMatrix(docs,control=list(tokenize=tetragram))
tetragramcorpus<-findFreqTerms(tetragramtab,lowfreq=1)
tetragramcorpusnum<-rowSums(as.matrix(tetragramtab[tetragramcorpus,]))
tetragramcorpustab<-data.frame(Word=names(tetragramcorpusnum),frequency=tetragramcorpusnum)
tetragramcorpussort<-tetragramcorpustab[order(-tetragramcorpustab$frequency),]
tetragramfinal<- separate(tetragramcorpussort, Word,c("word1", "word2","word3","word4"), sep=" ")

## Savings ngrams
setwd("C:/Users/Luis/Documents/Capstone Project/final/en_US_ngrams")

saveRDS(unigramcorpussort, file = "unigram.rds")
saveRDS(bigramfinal, file = "bigram.rds")
saveRDS(trigramfinal, file = "trigram.rds")
saveRDS(tetragramfinal, file = "tetragram.rds")

## Read n-grams

unigram<- readRDS(file = "C:/Users/Luis/Documents/Capstone Project/final/en_US_ngrams/unigram.rds")
bigram<- readRDS(file = "C:/Users/Luis/Documents/Capstone Project/final/en_US_ngrams/bigram.rds")
trigram<- readRDS(file = "C:/Users/Luis/Documents/Capstone Project/final/en_US_ngrams/trigram.rds")
tetragram<- readRDS(file = "C:/Users/Luis/Documents/Capstone Project/final/en_US_ngrams/tetragram.rds")

next2word <- function( w1){
  nextwordfilter<- filter(bigram, word1==w1)
  nextwordgram<- nextwordfilter[1:3,2]
  nextwordgram
}

next3word <- function( w1, w2){
  nextwordfilter<- filter(trigram, word1==w1& word2==w2)
  nextwordgram<- nextwordfilter[1:3,3]
  nextwordgram
}

next4word <- function( w1, w2,w3){
  nextwordfilter<- filter(tetragram, word1==w1&word2==w2&word3==w3)
  nextwordgram<- nextwordfilter[1:3,4]
  nextwordgram
}

teste<- function(x, y, z){
if (!is.na(next4word("x", "y", "z")[1])){
  print((next4word("x", "y", "z")[1]))
  }
  else{ if(!is.na(next3word("y", "z")[1])){
        print((next3word("y", "z")[1])) 
        }
        else{ if((!is.na(next2word("z")[1]))){
              print((next2word("z")[1]))
              }
              else{
                  print("Not Found")
                  }
            } 

  }
}



toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "-")
docs <- tm_map(docs, toSpace, ":")
docs <- tm_map(docs, toSpace, "'")
docs <- tm_map(docs, toSpace, "'")
docs <- tm_map(docs, toSpace, " -")
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, stripWhitespace)
docs <- tm_map(docs, PlainTextDocument)



custom_input_text_clean <- function (testline) 
{
  line <- iconv(testline, "latin1", "ASCII", sub = "")
  line <- gsub('[0-9]+', '', line)
  line <- tolower(line)
  line <- removePunctuation(line)
  line <- gsub('\\S+[^\x20-\x7E]', '', line)
  emptyLines <- grepl('^\\s*$', line)
  line <- line[!emptyLines]
  line <- stripWhitespace(line)
  #line <- gsub("^ +| +$|( ) +", "\\1", line)
  return(line)
}

line<- custom_input_text_clean("teste para hoje ver")

line <- VCorpus(VectorSource(line))

words<-function(x) NGramTokenizer(x,Weka_control(min=1,max=1))
wordstab<-TermDocumentMatrix(line,control=list(tokenize=words))
wordscorpus<-findFreqTerms(wordstab,lowfreq=0)
wordslen<-length(wordscorpus)
wordscorpus[wordslen]
wordscorpus[(wordslen-1)]
wordscorpus[(wordslen-2)]


###TESTING APP


library("shiny")
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library("ggplot2")
library("dplyr")
library("tidyr")
library("reshape2")
library("reshape")
library("RWeka")
library("quanteda")

    custom_input_text_clean <- function (testline) 
    {
      line <- iconv(testline, "latin1", "ASCII", sub = "")
      line <- gsub('[0-9]+', '', line)
      line <- tolower(line)
      line <- removePunctuation(line)
      line <- gsub('\\S+[^\x20-\x7E]', '', line)
      emptyLines <- grepl('^\\s*$', line)
      line <- line[!emptyLines]
      line <- stripWhitespace(line)
      #line <- gsub("^ +| +$|( ) +", "\\1", line)
      return(line)
    }
    
    line<- custom_input_text_clean("My name is worth done")
    
    tline <- strsplit(line, " ")[[1]]
    tline<- unique(tolower(tline))
    wordslen<-length(tline)
    t1<- tline[wordslen]
    t2<- tline[(wordslen-1)]
    t3<- tline[(wordslen-2)]
    
    unigram<- readRDS(file = "C:/Users/Luis/Documents/Capstone Project/final/en_US_ngrams/unigram.rds")
    bigram<- readRDS(file = "C:/Users/Luis/Documents/Capstone Project/final/en_US_ngrams/bigram.rds")
    trigram<- readRDS(file = "C:/Users/Luis/Documents/Capstone Project/final/en_US_ngrams/trigram.rds")
    tetragram<- readRDS(file = "C:/Users/Luis/Documents/Capstone Project/final/en_US_ngrams/tetragram.rds")
    
    next2word <- function( w1){
      nextwordfilter<- filter(bigram, word1==w1)
      nextwordgram<- nextwordfilter[1:3,2]
      nextwordgram
    }
    
    next3word <- function( w1, w2){
      nextwordfilter<- filter(trigram, word1==w1& word2==w2)
      nextwordgram<- nextwordfilter[1:3,3]
      nextwordgram
    }
    
    next4word <- function( w1, w2,w3){
      nextwordfilter<- filter(tetragram, word1==w1&word2==w2&word3==w3)
      nextwordgram<- nextwordfilter[1:3,4]
      nextwordgram
    }
    
    teste<- function(x, y, z){
      if ((!is.na(next4word(x, y, z)[1]))){
        print((next4word(x, y, z)[1]))
      }
      else{ if((!is.na(next3word(y, z)[1]))){
        print((next3word(y, z)[1])) 
      }
        else{ if((!is.na(next2word(z)[1]))){
          print((next2word(z)[1]))
        }
          else{
            print("Not Found")
          }
        } 
        
      }
    }
    
    
    unigram<- readRDS(file = "C:/Users/Luis/Documents/Capstone Project/ShinyApp/CapstoneProject/unigram.rds")
    bigram<- readRDS(file = "C:/Users/Luis/Documents/Capstone Project/ShinyApp/CapstoneProject/bigram.rds")
    trigram<- readRDS(file = "C:/Users/Luis/Documents/Capstone Project/ShinyApp/CapstoneProject/trigram.rds")
    tetragram<- readRDS(file = "C:/Users/Luis/Documents/Capstone Project/ShinyApp/CapstoneProject/tetragram.rds")
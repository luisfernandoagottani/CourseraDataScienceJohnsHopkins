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
library("data.table")

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

ltcorpus <- en_twitter

## Returns a named vector of n-grams and their associated frequencies
## extracted from the character vector dat.
##
## ng - Defines the type of n-gram to be extracted: unigram if ng=1,
##      bigram if ng=2, trigram if n=3, etc.
## dat - Character vector from which we want to get n-gram counts.
## ignores - Character vector of words (features) to ignore from frequency table
## sort.by.ngram - sorts the return vector by the names
## sort.by.freq - sorts the return vector by frequency/count
getNgramFreqs <- function(ng, dat, ignores=NULL,
                          sort.by.ngram=TRUE, sort.by.freq=FALSE) {
  # http://stackoverflow.com/questions/36629329/
  # how-do-i-keep-intra-word-periods-in-unigrams-r-quanteda
  if(is.null(ignores)) {
    dat.dfm <- dfm(dat, ngrams=ng, toLower = FALSE, removePunct = FALSE,
                   what = "fasterword", verbose = FALSE)
  } else {
    dat.dfm <- dfm(dat, ngrams=ng, toLower = FALSE, ignoredFeatures=ignores,
                   removePunct = FALSE, what = "fasterword", verbose = FALSE)
  }
  rm(dat)
  # quanteda docfreq will get the document frequency of terms in the dfm
  ngram.freq <- docfreq(dat.dfm)
  if(sort.by.freq) { ngram.freq <- sort(ngram.freq, decreasing=TRUE) }
  if(sort.by.ngram) { ngram.freq <- ngram.freq[sort(names(ngram.freq))] }
  rm(dat.dfm)
  
  return(ngram.freq)
}

## Returns a 2 column data.table. The first column: ngram, contains all the
## unigrams, bigrams, or trigrams in the corpus depending on whether
## ng = 1, 2, or 3 respectively. The second column: freq, contains the
## frequency or count of the ngram found in linesCorpus.
##
## ng - Defines the type of n-gram to be extracted: unigram if ng=1,
##      bigram if ng=2, trigram if n=3, etc.
## linesCorpus - character vector: each element is a line from a corpus file
## prefixFilter - character vector: If not NULL, tells the function to return
##                only rows where the ngram column starts with prefixFilter.
##                If NULL, returns all the ngram and count rows.
getNgramTables <- function(ng, linesCorpus, prefixFilter=NULL) {
  ngrams <- getNgramFreqs(ng, linesCorpus)
  ngrams_dt <- data.table(ngram=names(ngrams), freq=ngrams)
  if(length(grep('^SOS', ngrams_dt$ngram)) > 0) {
    ngrams_dt <- ngrams_dt[-grep('^SOS', ngrams_dt$ngram),]
  }
  if(!is.null(prefixFilter)) {
    regex <- sprintf('%s%s', '^', prefixFilter)
    ngrams_dt <- ngrams_dt[grep(regex, ngrams_dt$ngram),]
  }
  
  return(ngrams_dt)
}

unigs <- getNgramTables(1, ltcorpus)
bigrs <- getNgramTables(2, ltcorpus)
trigs <- getNgramTables(3, ltcorpus)
unigs; bigrs; trigs
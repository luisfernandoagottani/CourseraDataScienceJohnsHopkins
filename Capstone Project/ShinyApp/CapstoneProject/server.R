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

shinyServer(function(input, output) {
    output$predictedWord <- renderText({
      
        ##Cleaning input text
        
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
      
      line<- custom_input_text_clean(input$textInput)
      
      if (identical(line,character(0))){
        print("Type something, what are you waiting for?")}
      else {
      line <- strsplit(line, " ")[[1]]
      line<- unique(tolower(line))}
      
      wordslen<-length(line)
      
      t1<- line[wordslen]
      
      if(wordslen==1){
        t2=0} 
      else {t2<- line[(wordslen-1)]}
      
      if(wordslen<=2){
        t3=0} 
      else {t3<- line[(wordslen-2)]}
      
      unigram<- readRDS(file = "unigram.rds")
      bigram<- readRDS(file = "bigram.rds")
      trigram<- readRDS(file = "trigram.rds")
      tetragram<- readRDS(file = "tetragram.rds")
      
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
        
        if (identical(line,character(0))){
          print("Type something, what are you waiting for?")
        }
        else{ if ((!is.na(next4word(x, y, z)[1]))&wordslen>=3){
          print((next4word(x, y, z)[1]))
        }
          else{ if((!is.na(next3word(y, z)[1]))&wordslen>=2){
            print((next3word(y, z)[1])) 
          }
            else{ if((!is.na(next2word(z)[1]))){
              print((next2word(z)[1]))
            }
              else{
                print("Next word not Found")
              }
            }
          
          }
        }
        }
      if (identical(line,character(0))){
        print("Type something, what are you waiting for?")}
      else {teste(t3,t2,t1)}
    })      
    })
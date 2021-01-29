suppressMessages(library(dplyr))
suppressMessages(library(plyr))
suppressMessages(library(R.utils))
suppressMessages(library(tm))
suppressMessages(library(wordcloud))
suppressMessages(library(SnowballC))
suppressMessages(library(ggplot2))
suppressMessages(library(RWeka))
suppressMessages(library(textcat))
suppressMessages(library(ff))
suppressMessages(library(bigmemory))
suppressMessages(library(qdap))
suppressMessages(library(textcat))
suppressMessages(library(data.table))
suppressMessages(library(wordnet))
suppressMessages(library(pacman))
p_load_gh('hrbrmstr/pluralize')
p_load(quanteda)
suppressMessages(library(markovchain))
suppressMessages(library(R.cache))

## Reproducible


## Data Set
file_head <- function(name, n = 3) {
  connection <- file(name, open = "r")
  result <- readLines(connection, n = n)
  close(connection)
  result
}

## First contact with the Data
file_head("C:/Users/Luis/Documents/Capstone Project/final/en_US/en_US.blogs.txt")
file_head("C:/Users/Luis/Documents/Capstone Project/final/en_US/en_US.news.txt")
file_head("C:/Users/Luis/Documents/Capstone Project/final/en_US/en_US.twitter.txt")

## Analysing size

file_size <- function(name)
  suppressWarnings(utils:::format.object_size(file.info(name)[["size"]], "auto"))

file_lines <- function(name)
  suppressWarnings(countLines(name))

longest_line <- function(name)
  suppressWarnings(max(sapply(readLines(name), nchar)))

files_info <- function(dir) {
  files <- list.files(dir, recursive = TRUE, full.names = TRUE)
  data.frame(size = sapply(files, file_size),
             lines = sapply(files, file_lines),
             longest_line = sapply(files, longest_line))
}

files_info("C:/Users/Luis/Documents/Capstone Project/final/en_US")

## Reading Data

sample_file <- function(input_file, sample_fraction, test_fraction) {
  connection <- file(input_file, open = "r")
  lines <- readLines(connection, skipNul = TRUE)
  number_lines <- length(lines)
  close(connection)
  sample_indexes <- sample(1:number_lines, as.integer(number_lines * sample_fraction), replace = FALSE)
  partitioning_index <- as.integer(length(sample_indexes) * (1 - test_fraction))
  list(
    lines[sample_indexes[1:partitioning_index]],
    lines[sample_indexes[(partitioning_index + 1):length(sample_indexes)]]
  )
}

create_sample_file <- function(input_file, output_dir, sample_fraction, test_fraction) {
  sample <- sample_file(input_file, sample_fraction, test_fraction)
  train_sample <- sample[[1]]
  test_sample <- sample[[2]]
  dir.create(paste0(output_dir, "/training"), recursive = TRUE, showWarnings = FALSE)
  dir.create(paste0(output_dir, "/testing"), recursive = TRUE, showWarnings = FALSE)
  writeLines(train_sample, paste0(output_dir, "/training/", basename(input_file)))
  writeLines(test_sample, paste0(output_dir, "/testing/", basename(input_file)))
}

create_sample_files <- function(input_dir, output_dir, sample_fraction, test_fraction) {
  files <- list.files(input_dir, recursive = TRUE, full.names = TRUE)
  invisible(sapply(files, create_sample_file, output_dir = output_dir,
                   sample_fraction = sample_fraction,
                   test_fraction = test_fraction))
}

create_sample_files("C:/Users/Luis/Documents/Capstone Project/final/en_US", "C:/Users/Luis/Documents/Capstone Project/final/en_US_Samples", 0.001, 0.3)

## Text Mining

corpus <- VCorpus(DirSource("C:/Users/Luis/Documents/Capstone Project/final/en_US_Samples/training"), readerControl = list(language = "en_US"))

## Cleaning

replacePunctuation <- content_transformer(function(x) gsub("[^[:alnum:][:space:]'`]", " ", x))

clean_corpus <- function(corpus) {
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, replacePunctuation)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus
}

corpus <- clean_corpus(corpus)

## Removing Curse Words

load_curse_words <- function(curse_words_url) {
  connection <- url(curse_words_url)
  lines <- readLines(connection)
  close(connection)
  lines
}

curse_words <- load_curse_words(
  "https://raw.githubusercontent.com/LDNOOBW/List-of-Dirty-Naughty-Obscene-and-Otherwise-Bad-Words/master/en"
)

corpus_without_curse_words <- tm_map(corpus, removeWords, curse_words)

## Create Corpus fuction

create_corpus <- function(data_dir) {
  corpus <- VCorpus(DirSource(data_dir), readerControl = list(language = "en_US"))
  corpus <- clean_corpus(corpus)
  tm_map(corpus, removeWords, curse_words)
}

## N-Grams

create_ngrams_data_frame <- function(document_term_matrix) {
  frequencies <- colSums(document_term_matrix)
  ngrams <- data.frame(ngram = names(frequencies), frequency = frequencies, stringsAsFactors = FALSE)
  ngrams <- arrange(ngrams, desc(frequency))
  rownames(ngrams) <- 1:length(frequencies)
  return(ngrams)
}

one_gram_tokenizer <- function(x) 
  NGramTokenizer(x, Weka_control(min = 1, max = 1))

document_term_matrix <- DocumentTermMatrix(corpus_without_curse_words,
                                           control = list(tokenize = one_gram_tokenizer,
                                                          wordLengths=c(1, Inf)))

one_grams <- create_ngrams_data_frame(as.matrix(document_term_matrix))
top_one_grams <- one_grams[1:50, ]
top_one_grams

## Bigram

two_ngram_tokenizer <- function(x) 
  NGramTokenizer(x, Weka_control(min = 2, max = 2))

document_term_matrix <- DocumentTermMatrix(corpus_without_curse_words,
                                           control = list(tokenize = two_ngram_tokenizer,
                                                          wordLengths=c(1, Inf)))

two_ngrams <- create_ngrams_data_frame(as.matrix(document_term_matrix))

top_two_ngrams <- two_ngrams[1:50, ]
top_two_ngrams

## Trigrams

three_ngram_tokenizer <- function(x) 
  NGramTokenizer(x, Weka_control(min = 3, max = 3))

document_term_matrix <- DocumentTermMatrix(corpus_without_curse_words,
                                           control = list(tokenize = three_ngram_tokenizer,
                                                          wordLengths=c(1, Inf)))

three_ngrams <- create_ngrams_data_frame(as.matrix(document_term_matrix))

top_three_ngrams <- three_ngrams[1:50, ]
top_three_ngrams

## Fourgrams

four_gram_tokenizer <- function(x) 
  NGramTokenizer(x, Weka_control(min = 4, max = 4))

document_term_matrix <- DocumentTermMatrix(corpus_without_curse_words,
                                           control = list(tokenize = four_gram_tokenizer,
                                                          wordLengths=c(1, Inf)))

four_ngrams <- create_ngrams_data_frame(as.matrix(document_term_matrix))
four_ngrams[1: 50, ]

## Word Coverage

words_coverage <- data.frame(
  coverage = round(cumsum(one_grams$frequency) / sum(one_grams$frequency) * 100, 2),
  words = 1:nrow(one_grams)
)

words_coverage[1:500, ]

## 50%

number_ngrams_for_50_percent_coverage <- min(words_coverage[words_coverage$coverage > 50, ]$words)
number_ngrams_for_50_percent_coverage

## 90%

number_ngrams_for_90_percent_coverage <- min(words_coverage[words_coverage$coverage > 90, ]$words)
number_ngrams_for_90_percent_coverage

## Prediction model
create_sample_files("C:/Users/Luis/Documents/Capstone Project/final/en_US", "C:/Users/Luis/Documents/Capstone Project/final/en_US_SamplesPM", 0.002, 0.3)
corpus_without_curse_words <- create_corpus("C:/Users/Luis/Documents/Capstone Project/final/en_US_SamplesPM/training")

## Add UKN WORDS

add_unknown_words <- function(corpus) {
  document_term_matrix <- 
    DocumentTermMatrix(corpus,
                       control = list(tokenize = function(x) NGramTokenizer(x, Weka_control(min = 1, max = 1)),
                                      wordLengths=c(1, Inf)))
  words <- names(colSums(as.matrix(document_term_matrix)))
  
  counter <- 0
  percentage_counter <- 0
  percentage_complete <- 0
  for(word in words) {
    
    document_index <- which.max(tm_index(corpus, FUN = function(x) any(grep(paste0("\\b", word,"\\b"), content(x)))))
    line_index <- which.max(grepl(paste0("\\b", word, "\\b"), content(corpus[[document_index]])))
    content(corpus[[document_index]])[line_index] <-
      sub(paste0("\\b", word, "\\b"), "<unknown>",
          content(corpus[[document_index]])[line_index])
    
    counter <- counter + 1
    percentage_counter <- round(counter * 100 / length(words))
    if(percentage_counter > percentage_complete & percentage_counter < 100) {
      percentage_complete <- percentage_counter
      print(paste0(percentage_complete, "% complete."))
    }
  }
  print("complete.")
  corpus
}

corpus_without_curse_words <- add_unknown_words(corpus_without_curse_words)

## Katz Back Off Implementation

create_frequencies <- function(corpus, min_count) {
  one_to_four_gram_tokenizer <- function(x) 
    NGramTokenizer(x, Weka_control(min = 1, max = 8))
  document_term_matrix <- DocumentTermMatrix(corpus,
                                             control = list(tokenize = one_to_four_gram_tokenizer,
                                                            wordLengths=c(1, Inf)))
  ngrams <- create_ngrams_data_frame(as.matrix(document_term_matrix))
  ngrams$frequency <- sapply(ngrams$frequency, function(x) x - 1)
  ngrams <- rbind(ngrams, data.frame(ngram = "<UNKNOWN>", frequency = nrow(ngrams)))
  data.table(subset(ngrams, frequency > min_count))
}

frequencies_dt <- create_frequencies(corpus_without_curse_words, 0)
sample_n(frequencies_dt, 12)

## Last token to optimazation

extract_history <- function(ngram){
  ifelse(length(ngram) > 1,
         paste(ngram[1:(length(ngram)-1)], collapse = " "),
         ""
  )
}

extract_word <- function(ngram) paste(ngram[length(ngram)], collapse = " ")

build_processed_ngram_frequencies <- function(frequencies_dt) {
  data <- as.data.frame(frequencies_dt)
  data$ngram <- strsplit(data$ngram, split = " |'")
  data$ngram_length <- sapply(data$ngram, length)
  data$history <- sapply(data$ngram, extract_history)
  data$word <- sapply(data$ngram, extract_word)
  data$ngram <- sapply(data$ngram, paste, collapse = " ")
  data.table(data)
}

ngram_frequencies_dt <- build_processed_ngram_frequencies(frequencies_dt)
as.data.frame(ngram_frequencies_dt[ngram_length > 1, ][order(-frequency), ][1:50, ])

## Probrabilities

history_frequencies_dt <-
  ngram_frequencies_dt[, c("history", "frequency")][, lapply(.SD, sum), by = list(history)]

as.data.frame(history_frequencies_dt[order(-frequency), ][1:50])

## size of obs.

frequencies_of_frequencies <- table(ngram_frequencies_dt$frequency)
frequencies_of_frequencies

## Good Turing Formula

frequency_of_frequency <- function(frequency, frequencies_of_frequencies)
  try_default(frequencies_of_frequencies[[toString(frequency)]], 1, quiet = TRUE)

discount <- function(count, frequencies_of_frequencies) {
  good_turing_count <- (count + 1) *
    frequency_of_frequency(count + 1, frequencies_of_frequencies) /
    frequency_of_frequency(count, frequencies_of_frequencies)
  computed_discount <- good_turing_count / count
  ifelse(computed_discount < 1, computed_discount, 1)
}

discount(1, frequencies_of_frequencies)

## Katz Prob

count_ngram <- function(word_value, history_value, ngram_frequencies_dt) {
  count <- ngram_frequencies_dt[word == word_value & history == history_value, ]$frequency
  ifelse(length(count) > 0, count, 1)
}

count_history <- function(history_value, history_frequencies_dt) {
  count <- history_frequencies_dt[history == history_value, ]$frequency
  ifelse(length(count) > 0, count, 1)
}

total_words <- sum(ngram_frequencies_dt[ngram_length == 1, ]$frequency)

count_ngram("t", "don", ngram_frequencies_dt)

count_history("don", history_frequencies_dt)

backoff_history <- function(history) {
  history_words <- strsplit(history, split = " |'")
  ifelse(
    length(history_words[[1]]) > 1,
    trimws(paste(backoff_history_words <- history_words[[1]][2:length(history_words[[1]])], collapse = " ")),
    ""
  )
}

backoff_history("bird is the word")


## Katz Beta

katz_beta <- function(history_value, k,
                      ngram_frequencies_dt,
                      history_frequencies_dt,
                      frequencies_of_frequencies) {
  
  counts <- ngram_frequencies_dt[history == history_value & frequency > k, ]$frequency
  history_count <- count_history(history_value, history_frequencies_dt)
  1 - ifelse(length(counts) > 0,
             sum(
               sapply(counts,
                      function(x) discount(x, frequencies_of_frequencies) * x / history_count
               )
             ),
             0
  )
}

memoized_katz_beta <- addMemoization(katz_beta)

memoized_katz_beta("didn", 5,
                   ngram_frequencies_dt,
                   history_frequencies_dt,
                   frequencies_of_frequencies)
## [1] 0.4946237

## Katz Alpha

katz_alpha_summation <- function(history_value, k,
                                 ngram_frequencies_dt,
                                 history_frequencies_dt,
                                 frequencies_of_frequencies) {
  words <- ngram_frequencies_dt[history == history_value & frequency <= k, ]$word
  ifelse(length(words) > 0,
         sum(
           sapply(words,
                  katz_probability,
                  history = backoff_history(history_value),
                  k = k,
                  ngram_frequencies_dt = ngram_frequencies_dt,
                  history_frequencies_dt = history_frequencies_dt,
                  frequencies_of_frequencies = frequencies_of_frequencies
           )
         ),
         0
  )
}

memoized_katz_alpha_summation <- addMemoization(katz_alpha_summation)

katz_alpha <- function(history, k,
                       ngram_frequencies_dt,
                       history_frequencies_dt,
                       frequencies_of_frequencies) {
  
  computed_katz_alpha_summation <- memoized_katz_alpha_summation(history, k,
                                                                 ngram_frequencies_dt,
                                                                 history_frequencies_dt,
                                                                 frequencies_of_frequencies
  )
  
  computed_katz_beta <- memoized_katz_beta(history, k,
                                           ngram_frequencies_dt,
                                           history_frequencies_dt,
                                           frequencies_of_frequencies
  )
  
  computed_katz_alpha <- ifelse(computed_katz_alpha_summation != 0,
                                computed_katz_beta / computed_katz_alpha_summation,
                                1)
  ifelse(computed_katz_alpha < 1, computed_katz_alpha, 1)
}

memoized_katz_alpha <- addMemoization(katz_alpha)

katz_probability <- function(word, history, k,
                             ngram_frequencies_dt,
                             history_frequencies_dt,
                             frequencies_of_frequencies,
                             verbose = FALSE) {
  if(verbose) print(paste0("katz_probability(word: [", word, "], history: [", history, "])..."))
  
  word_with_history <- trimws(paste(history, word))
  count <- count_ngram(word, history, ngram_frequencies_dt)
  
  probability <- ifelse(
    history == "",
    discount(count, frequencies_of_frequencies) * count / total_words,
    ifelse(count > k,
           discount(count, frequencies_of_frequencies) * count / count_history(history, ngram_frequencies_dt),
           memoized_katz_alpha(history, k, ngram_frequencies_dt, history_frequencies_dt, frequencies_of_frequencies) *
             katz_probability(word, backoff_history(history), k,
                              ngram_frequencies_dt,
                              history_frequencies_dt,
                              frequencies_of_frequencies,
                              verbose = verbose)
    )
  )
  
  if(verbose) print(paste0("katz_probability(word: [", word, "], history: [", history, "]) = ", probability))
  probability
}

katz_alpha_summation("didn", 5, ngram_frequencies_dt, history_frequencies_dt, frequencies_of_frequencies)

## Markov Chain Matrix

create_transition_matrix <- function(k,
                                     ngram_frequencies_dt,
                                     history_frequencies_dt,
                                     frequencies_of_frequencies,
                                     min_count = 3, verbose = FALSE) {
  
  prediction_data <- ngram_frequencies_dt[ngram_length > 1 & frequency > min_count, ]
  
  histories <- unique(c("", prediction_data$history))
  print(paste0("history size: ", length(histories)))
  
  words <- unique(prediction_data$word)
  print(paste0("word size: ", length(words)))
  
  transition_matrix <- matrix(NA, length(histories), length(words))
  rownames(transition_matrix) <- histories
  colnames(transition_matrix) <- words
  
  percentage_complete <- 0
  percentage_counter <- 0
  for(i in 1:length(histories)){
    for(j in 1:length(words)){
      transition_matrix[i, j] <- katz_probability(words[[j]],
                                                  histories[[i]],
                                                  k,
                                                  ngram_frequencies_dt,
                                                  history_frequencies_dt,
                                                  frequencies_of_frequencies,
                                                  verbose = verbose)
    }
    percentage_complete <- round((i * length(words) + j + 1) / (length(histories) * length(words)) * 100)
    if(percentage_complete > percentage_counter & percentage_complete < 100) { 
      percentage_counter <- percentage_complete
      print(paste0(percentage_counter, "% complete."))
    }
  }
  print("complete.")
  transition_matrix
}

model <- create_transition_matrix(5,
                                  ngram_frequencies_dt,
                                  history_frequencies_dt,
                                  frequencies_of_frequencies)
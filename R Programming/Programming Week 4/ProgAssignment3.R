##Plot the 30-day mortality rates for heart attack

outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

head(outcome)

outcome[, 11] <- as.numeric(outcome[, 11])

##To make a simple histogram of the 30-day death rates from heart attack (column 11 in the outcome dataset):

outcome[, 11] <- as.numeric(outcome[, 11])

hist(outcome[, 11])



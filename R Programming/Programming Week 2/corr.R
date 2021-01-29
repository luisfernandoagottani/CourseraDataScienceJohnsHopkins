## 'directory' is a character vector of length 1 indicating
## the location of the CSV files

## 'threshold' is a numeric vector of length 1 indicating the
## number of completely observed observations (on all
## variables) required to compute the correlation between
## nitrate and sulfate; the default is 0

## Return a numeric vector of correlations
## NOTE: Do not round the result!

corr <- function(directory, threshold = 0) {
  files <- list.files(directory, full.names = TRUE)
  dat <- vector(mode = "numeric", length = 0)
  
  for(i in 1:length(files)) {
    moni_i<- read.csv(files[i])
    csum <- sum((!is.na(moni_i$sulfate)) & (!is.na(moni_i$nitrate)))
    if (csum > threshold) {
      tmp <- moni_i[which(!is.na(moni_i$sulfate)),]
      submoni_i <- tmp [which(!is.na(tmp$nitrate)),]
      dat <- c(dat,cor(submoni_i$sulfate, submoni_i$nitrate))
    }
  }
  
  dat
}
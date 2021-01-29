## 'directory' is a character vector of length 1 indicating
## the location of the CSV files

## 'id' is an integer vector indicating the monitor ID numbers
## to be used

## Return a data frame of the form:
## id nobs
## 1  117
## 2  1041
## ...
## where 'id' is the monitor ID number and 'nobs' is the
## number of complete cases

complete <- function(directory, id = 1:332){
  files<- list.files(directory, full.names = TRUE)
  dat<- data.frame() ##Indicating data.frame() for rbind
  
  for(i in id) {
    moni_i <- read.csv(files[i])
    nobs<- sum(complete.cases(moni_i))
    tmp<- data.frame(i, nobs)
    dat<- rbind(dat, tmp)
  }
  colnames(dat) <- c("id","nods")
  dat
}
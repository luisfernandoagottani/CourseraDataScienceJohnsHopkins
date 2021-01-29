##Q1
urlq4<- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"
download.file(urlq4, destfile = "C:/Users/Luis/Documents/Getting and Cleaning Data/Week 4/urlq4.csv")
fileq4<- read.csv("C:/Users/Luis/Documents/Getting and Cleaning Data/Week 4/urlq4.csv")

splitfile4<- strsplit(names(fileq4), "\\wgtp")

splitfile4[[123]]

##Q2

Q2_Url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv"
Q2_Path <- "C:/Users/Luis/Documents/Getting and Cleaning Data/Week 4/urlq4q2.csv"
download.file(Q2_Url, Q2_Path)

Q2_File<- read.csv(Q2_Path ,nrow=190, skip=4)
Q2_File<- Q2_File[, c(1,2,4,5)]
colnames(Q2_File) <- c("CountryCode", "Rank", "Country", "Total")

Q2_File$Total <- as.integer(gsub(",", "", Q2_File$Total))
mean(Q2_File$Total, na.rm = T)

##Q3

Q2_File$Country<- Q2_File$Country
Q2_File$Country[grep("^United", Q2_File$Country)]

##Q4

Q4a_Url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv"
Q4a_Path <- "C:/Users/Luis/Documents/Getting and Cleaning Data/Week 4/urlq4q4a.csv"
download.file(Q4a_Url, Q4a_Path)

Q4a_File<- fread(Q4a_Path ,nrow=190, skip=4)
Q4a_File<- Q4a_File[, c(1,2,4,5)]
colnames(Q4a_File) <- c("CountryCode", "Rank", "Country", "Total")

Q4b_Url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv"
Q4b_Path <- "C:/Users/Luis/Documents/Getting and Cleaning Data/Week 4/urlq4q4b.csv"
download.file(Q4b_Url, Q4b_Path)

Q4b_File<- fread(Q4b_Path)

Qmerge<- merge(Q4a_File, Q4b_File, by='CountryCode')

FiscalJune <- grep("Fiscal year end: June", Qmerge$`Special Notes`)

NROW(FiscalJune)

##Q5
library(quantmod)
amzn = getSymbols("AMZN",auto.assign=FALSE)
sampleTimes = index(amzn)

#2012
amzn2012 <- sampleTimes[grep("^2012", sampleTimes)]
NROW(amzn2012)

#on mondays 2012
NROW(amzn2012[weekdays(amzn2012) == "Monday"])


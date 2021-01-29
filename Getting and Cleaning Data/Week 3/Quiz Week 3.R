##Q1
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"
f <- file.path(getwd(), "Quiz3_Q1.csv")
download.file(url, f)
Q1<- (read.csv(url))

agricultureLogical <- Q1$ACR == 3 & Q1$AGS == 6
which(agricultureLogical)

#Q2

library(jpeg)

Q2Url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fjeff.jpg"
Q2Path = 'getwd(), Q2Url.jpg'
download.file(Q2Url, Q2Path, mode = 'wb')
Q2 <- readJPEG(Q2Path, native = TRUE)
quantile(Q2, probs = c(0.3, 0.8))

#Q3

Q3aUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv"
Q3aPath = 'getwd(), Q3aUrl.jpg'

Q3bUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv"
Q3bPath = 'getwd(), Q3bUrl.jpg'

download.file(Q3aUrl, Q3aPath, method = 'curl')
download.file(Q3bUrl, Q3bPath, method = 'curl')
##ORGANIZANDO DATA
Q3a<- fread(Q3aPath, skip=4, nrows=191, select = c(1,2,4,5), col.names= c("CountryCode", "Rank", "Economy","Total"))
Q3b<- fread(Q3bPath)

Q3m<- merge(Q3a, Q3b, by='CountryCode')
Q3m<- Q3m %>% arrange(desc(Rank))

paste(nrow(Q3m), "matches, 13th country is ",Q3m$Economy[13])

##Q4

Q3m %>% group_by(`Income Group`) %>%
  filter("High income: OECD" %in% `Income Group` | "High income: nonOECD" %in% `Income Group`) %>%
  summarize(Average = mean(Rank, na.rm = T)) %>%
  arrange(desc(`Income Group`))

##Q5

Q3m$RankGroups <- cut(Q3m$Rank, breaks = 5)
vs <- table(Q3m$RankGroups, Q3m$`Income Group`)
vs
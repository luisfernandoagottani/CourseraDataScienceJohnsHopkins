##QUESTION 1
URL_QUIZ1<- ("https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv") 

download.file(URL_QUIZ1, destfile="../Week 1/UnitedStatesCommunities.csv")

USAC<- read.table("../Week 1/UnitedStatesCommunities.csv", sep= ",", header=TRUE)

DTUSAC <- data.table(USAC)

DTUSAC[VAL==24, .N]

##QUESTION 3
URL_QUESTION3<- ("https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FDATA.gov_NGAP.xlsx")

download.file(URL_QUESTION3, destfile= "../Week 1/NaturalGasAP.xlsx", mode='wb')

rowIndex<- 18:23
colIndex<- 7:15
dat<- read.xlsx("../Week 1/NaturalGasAP.xlsx", sheetIndex=1
                 , colIndex=colIndex, rowIndex=rowIndex, header= TRUE)

##QUESTION 4

fileUrl<- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Frestaurants.xml"

doc<- xmlTreeParse(sub("s", "", fileUrl), useInternal=TRUE)

rootNode <- xmlRoot(doc)

zipcodes <- xpathSApply(rootNode, "//zipcode", xmlValue)

xmlZipcodeDT <- data.table(zipcodes)

xmlZipcodeDT [zipcode == 21231, .N]

##QUESTION 5

URL_QUIZ5<- ("https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06pid.csv") 

download.file(URL_QUIZ5, destfile="../Week 1/UnitedStatesCommunitiesSurvey.csv")

DT<- fread("../Week 1/UnitedStatesCommunitiesSurvey.csv", sep= ",", header=TRUE)

DT[,mean(pwgtp15),by=SEX]

filename <- "Data_for_Peer_Assessment.zip"

#File exists? -No - (Download)
if (!file.exists(filename)){
  fileURL <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
  download.file(fileURL, filename)
}
#Folder unzip? - No - (Download)
if (!file.exists("Data_for_Peer_Assessment.zip")) { 
  unzip(filename)
}  
##Reading file as .RDS

NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

##Have total emissions from PM2.5 decreased in the United States from 
##1999 to 2008? Using the base plotting system, make a plot showing the 
##total PM2.5 emission from all sources for each of the years 1999, 2002, 
##2005, and 2008.

##SEPARATING DATA
library(dplyr)
NEITotal<- NEI %>% 
  group_by(year) %>% 
  summarise(Emissions = sum(Emissions))

##Plot1.png

# 1. Open png file
png(filename= "plot1.png", width = 480, height = 480, unit="px")
# 2. Create the plot
Plot1.R<- barplot(height= NEITotal$Emissions/1000, 
                  names.arg= NEITotal$year, 
                  xlab="year",
                  ylab="Total emission of PM2.5 in USA (Kilotons)",
                  main="Total emission of PM2.5 for year in USA",
                  ylim=c(0,8000),
                  col=c("black", "grey", "blue", "green"),
)
## Add text at top of bars
text(x = Plot1.R, 
     y = round(NEITotal$Emissions/1000,2), 
     label = round(NEITotal$Emissions/1000,2), 
     pos = 3, 
     cex = 0.8, 
     col = "black")
# 3. Close the file
dev.off()
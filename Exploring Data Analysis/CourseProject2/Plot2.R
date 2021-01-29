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

##Have total emissions from PM2.5 decreased in the Baltimore City, Maryland 
##(\color{red}{\verb|fips == "24510"|}fips=="24510") 
##from 1999 to 2008? Use the base plotting system to 
##make a plot answering this question.

str(NEI)

NEI_Balt_City<- subset(NEI, NEI$fips=="24510")

NEI_Balt_CityTotal<- NEI_Balt_City %>% 
  group_by(year) %>% 
  summarise(Emissions = sum(Emissions))

##Plot2.png

# 1. Open png file
png(filename= "plot2.png", width = 480, height = 480, unit="px")
# 2. Create the plot
Plot2.R<- barplot(height= NEI_Balt_CityTotal$Emissions, 
                  names.arg= NEI_Balt_CityTotal$year, 
                  xlab="year",
                  ylab="Total emission of PM2.5 in Baltimore City, Maryland (tons)",
                  main="Total emission of PM2.5 for year in Baltymore City, Maryland",
                  ylim=c(0,4000),
                  col=c("black", "grey", "blue", "green"),
)
## Add text at top of bars
text(x = Plot2.R, 
     y = round(NEI_Balt_CityTotal$Emissions,2), 
     label = round(NEI_Balt_CityTotal$Emissions,2), 
     pos = 3, 
     cex = 0.8, 
     col = "black")
# 3. Close the file
dev.off()
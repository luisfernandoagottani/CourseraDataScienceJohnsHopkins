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

##Compare emissions from motor vehicle sources in Baltimore
##City with emissions from motor vehicle sources in Los Angeles County, 
##California (\color{red}{\verb|fips == "06037"|}fips=="06037"). 
##Which city has seen greater changes over time in motor vehicle emissions?

NEI_Balt<- subset(NEI, NEI$fips=="24510"&NEI$type=="ON-ROAD")

NEI_BaltF<- NEI_Balt %>% 
  group_by(year) %>% 
  summarise(Emissions = sum(Emissions))

NEI_LA<- subset(NEI, NEI$fips=="06037"&NEI$type=="ON-ROAD")

NEI_LAF<- NEI_LA %>% 
  group_by(year) %>% 
  summarise(Emissions = sum(Emissions))

NEI_BaltF$County <- "Baltimore City, MD"
NEI_LAF$County <- "Los Angeles County, CA"

NEI_BOTH <- rbind(NEI_BaltF, NEI_LAF)

##Plot6.png

# 1. Open png file
png(filename= "plot6.png", width = 600, height = 600, unit="px")
# 2. Create the plot
ggplot(NEI_BOTH, 
       aes(x=factor(year), y=Emissions, fill=County,label = round(Emissions)))+
  geom_bar(stat="identity")+
  facet_grid(County~., scales="free") +
  xlab("year") +
  ylab(expression("PM25 emission in tons by motor vehicles sources")) +
  ggtitle(expression("Total emission of PM2.5 for year in Baltimore and Los Angeles by motor vehicles sources"))+
  geom_label(aes(fill = County), colour = "white", fontface = "bold")
# 3. Close the file
dev.off()
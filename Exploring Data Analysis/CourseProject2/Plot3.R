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

##Of the four types of sources indicated by the 
##\color{red}{\verb|type|}type (point, nonpoint, onroad, nonroad) 
##variable, which of these four sources have seen decreases in emissions 
##from 1999-2008 for Baltimore City? Which have seen increases in emissions
##from 1999-2008? Use the ggplot2 plotting system to make a plot answer this
##question.

str(NEI)

NEI_Balt_City<- subset(NEI, NEI$fips=="24510")

NEI_Balt_CityType<- NEI_Balt_City %>% 
  group_by(type, year) %>%
  summarise(Emissions = sum(Emissions))

##Plot3.png

# 1. Open png file
png(filename= "plot3.png", width = 1200, height = 480, unit="px")
# 2. Create the plot
ggplot(NEI_Balt_CityType, 
  aes(x=factor(year), y=Emissions, fill=type,label = round(Emissions,2)))+
geom_bar(stat="identity")+
facet_grid(. ~ type)+
xlab("year") +
ylab(expression("PM25 emission in tons")) +
ggtitle(expression("Total emission of PM2.5 for year in Baltymore City, Maryland by Type"))+
geom_label(aes(fill = type), colour = "white", fontface = "bold")
# 3. Close the file
dev.off()
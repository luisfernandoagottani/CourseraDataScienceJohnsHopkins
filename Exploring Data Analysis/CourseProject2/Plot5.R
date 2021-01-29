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

##How have emissions from motor vehicle sources changed from 1999-2008 
##in Baltimore City?

NEI_R<- subset(NEI, NEI$fips=="24510"&NEI$type=="ON-ROAD")

NEI_RT<- NEI_R %>% 
  group_by(year) %>% 
  summarise(Emissions = sum(Emissions))

##Plot5.png

# 1. Open png file
png(filename= "plot5.png", width = 480, height = 480, unit="px")
# 2. Create the plot
ggplot(NEI_RT, 
       aes(x=factor(year), y=Emissions, fill=year,label = round(Emissions)))+
  geom_bar(stat="identity")+
  xlab("year") +
  ylab(expression("PM25 emission in Baltimore, Maryland in tons by motor vehicles sources")) +
  ggtitle(expression("Total emission of PM2.5 for year in Baltimore, Maryland in tons by motor vehicles sources"))+
  geom_label(aes(fill = year), colour = "white", fontface = "bold")
# 3. Close the file
dev.off()
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

##Across the United States, how have emissions from coal combustion-related 
##sources changed from 1999-2008?

str(NEI)

SCC_F1<- grepl("Fuel Comb.*Coal", SCC$EI.Sector)

SCC_F2<- SCC[SCC_F1,]

NEI_Coal<- NEI[NEI$SCC%in%SCC_F2$SCC, ]

NEI_CoalSum<- NEI_Coal %>% 
  group_by(year) %>%
  summarise(Emissions = sum(Emissions))

##Plot4.png

# 1. Open png file
png(filename= "plot4.png", width = 480, height = 480, unit="px")
# 2. Create the plot
ggplot(NEI_CoalSum, 
       aes(x=factor(year), y=Emissions/1000, fill=year,label = round(Emissions/1000,2)))+
  geom_bar(stat="identity")+
  xlab("year") +
  ylab(expression("PM25 emission in kilotons by coal combustion-related")) +
  ggtitle(expression("Total emission of PM2.5 for year in USA by coal combustion-related"))+
  geom_label(aes(fill = year), colour = "white", fontface = "bold")
# 3. Close the file
dev.off()
library(ggplot2)
#this script will be focused on comparing the FDA data to trade journal data
data1 <- read.csv(file = "./data/business_innovation/working/PHARMACY_TIMES/OTC/otc_dirty.csv")
data2 <- read.csv(file = "./data/business_innovation/working/PHARMACY_TIMES/Rx/Rx_dirty.csv")
data <- rbind(data1,data2)
data <- as.data.frame(data)
rownames(data) <- seq(length=nrow(data))
data <- subset(data, (Year >= 2013))
data <- subset(data, (Year < 2016))


data <- data[,-1] #this is the full data set, all OTC and RX combined

ptoday <- read.csv(file = "./data/business_innovation/working/PHARMACY_TODAY/pToday.csv")



comp <- read.csv(file = './src/eiriki/newJournals/comparisons.xlsx - FDA vs. Trade Journals.csv')
compcomp <- comp[c(68,69,75,254,285,375,392),]
compcomp[,1] <- c("Boehringer Ingelheim", "Boehringer Ingelheim","Boehringer Ingelheim",
                  "AstraZeneca","AstraZeneca","Bristol-Myers Squibb","Astellas")
compcomp$FDA.Approval.Date <- as.Date(compcomp$FDA.Approval.Date, format="%m/%d/%y")
compcomp$PharmacyTimes.Date <- as.Date(compcomp$PharmacyTimes.Date, format="%Y-%m-%d")
compcomp$PharmacyToday.Date <- c(NA,"2014-11-1", "2014-11-1","2014-6-1",'2015-7-1','2015-11-1','2015-4-1')
#drop ingelheim 3
compcomp=compcomp[-3,]
compcomp['TimesDaysSince'] = difftime(compcomp$PharmacyTimes.Date,compcomp$FDA.Approval.Date, units = "days")
compcomp['TodayDaysSince'] = difftime(compcomp$PharmacyToday.Date,compcomp$FDA.Approval.Date, units = "days")

write.csv(compcomp, './src/eiriki/newJournals/comparisons.xlsx - FDA vs. Trade Journals2.csv')


##after cleaning in excel
graphing <- read.csv(file = './src/eiriki/newJournals/comparisons.xlsx - FDA vs. Trade Journals3.csv')
graphing <- graphing[,c(-1,-8,-9)]
graphing$Name <- paste(graphing$Company,graphing$Name, sep= ", ")
#clean out FDA
graphing <- subset(graphing,graphing$Source!='FDA')
graphing$Source <- as.factor(graphing$Source)
rownames(graphing) <- seq(1,19,1)
graphing <- graphing[1:12,]
rownames(graphing) <- seq(1,12,1)
graphing <- graphing[c(-1,-2),]


ggplot(graphing,aes(x=factor(Name),y=DaysSince,fill=Source)) + geom_bar(stat = "identity",position = "dodge") +
  coord_flip()+
  geom_text(size=8,aes(label=DaysSince),position=position_dodge(width=.9), vjust=-0.25)+
  theme_bw()+
  theme(title = element_text(size=20), axis.text.x=element_text(size=24, face = 'bold'), axis.text.y = element_text(size = 22, face = 'bold'),
        axis.title.x = element_text(size = 28, face = 'bold'),axis.title.y = element_text(size = 28, face = 'bold'))+
  theme(plot.title = element_text(size = 38, face = 'bold'))+
  theme(legend.text=element_text(size=20),legend.key.size = unit(1.25,'cm'))+
  theme(axis.text.x = element_text(angle = -35, hjust = 0)) + xlab("Company Name, Drug Name") +
  ylab("Days Since FDA Release") + ggtitle("Lag Time of Pharmaceutical Journals for Top Companies")


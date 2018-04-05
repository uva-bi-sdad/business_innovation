library(dplyr)
library(plyr)
library(stringr)
library(ggplot2)

data1 <- read.csv(file = "./data/business_innovation/working/PHARMACY_TIMES/OTC/otc_dirty.csv")
data2 <- read.csv(file = "./data/business_innovation/working/PHARMACY_TIMES/Rx/Rx_dirty.csv")
data <- rbind(data1,data2)
data <- as.data.frame(data)

data <- data[,-1] #this is the full data set, all OTC and RX combined
#subsets
compData <- subset(data, (Year >= 2013))
compData <- subset(compData, (Year < 2016))

compData$Company <- str_replace_all(compData$Company, ": ", "")
compData$Company <- str_replace_all(compData$Company, "(?<=(Baxter)).*", "")
compData$Company <- str_replace_all(compData$Company, "(?<=(Merck)).*", "")
compData$Company <- str_replace_all(compData$Company, "(?<=(Novartis)).*", "")
compData$Company <- str_replace_all(compData$Company, "(?<=(Pfizer)).*", "")
compData$Company <- str_replace_all(compData$Company, "(?<=(GlaxoSmithKline)).*", "")
compData$Company <- as.factor(compData$Company)

spreads <- compData[compData$Company == "Merck",]
spreads <- rbind(spreads,compData[compData$Company == "Novartis",])
spreads <- rbind(spreads,compData[compData$Company == "Baxter",])
spreads <- rbind(spreads,compData[compData$Company == "Pfizer",])
spreads <- rbind(spreads,compData[compData$Company == "GlaxoSmithKline",])
spreads$Company <- factor(spreads$Company)

#export csv for comparisons
rownames(spreads) <- seq(length=nrow(spreads))
#write.csv(spreads, file = "./data/business_innovation/working/PHARMACY_TIMES/pTimesInterest.csv")

max <- table(spreads$Company)
max <- as.data.frame(max)
colnames(max) <- c("Company", "Freq")
spreads <- table(spreads$Company, spreads$Year)
spreads <- as.data.frame(spreads)
colnames(spreads) <- c("Company", "Year", "Freq")
spreads <- subset(spreads,  Freq >= 1)
spreads["max"] = c(0)

for (i in 1:nrow(spreads)){
  for (k in 1:nrow(max)){
    if (spreads$Company[i] == max$Company[k]){
      spreads$max[i] = max$Freq[k]
    }
  }
}
spreads <- spreads[order(-spreads$max),]

#change the order with factor
spreads$Year <- factor(spreads$Year, levels = c(2013,2014,2015))
vis <- ggplot(spreads,aes(x= Company,y=Freq,fill = Year)) + geom_col(color = 'black',position = 'dodge') +
  geom_text(size=8,aes(label=Freq),position=position_dodge(width=.9), vjust=-0.25)+
  theme_bw()+
  theme(title = element_text(size=20), axis.text.x=element_text(size=24, face = 'bold'), axis.text.y = element_text(size = 22, face = 'bold'),
        axis.title.x = element_text(size = 28, face = 'bold'),axis.title.y = element_text(size = 28, face = 'bold'))+
  theme(plot.title = element_text(size = 38, face = 'bold'))+
  theme(legend.text=element_text(size=20),legend.key.size = unit(1.25,'cm'))+
  theme(axis.text.x = element_text(angle = -35, hjust = 0)) + xlab("Company") +
  ylab("Number of News Articles") + ggtitle("Companies of Interest in OTC and Rx 2013-2015")
vis

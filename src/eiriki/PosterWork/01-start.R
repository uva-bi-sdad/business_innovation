# Exploratory work
library(ggplot2)
library(dplyr)
library(stringr)
library(readxl)
source('./src/eiriki/PosterWork/cleaning.R')
#only applying cleaning to Drugs journals
pharmtimes <- read.csv('./data/business_innovation/working/PHARMACY_TIMES/combined/combined.csv',stringsAsFactors = F)
ptoday <- read.csv('./data/business_innovation/working/PHARMACY_TODAY/pToday.csv')
FDA_Drugs <- read_xlsx('./data/business_innovation/original/fda_drugs/Copy of FDA Database - COMBINED V2.xlsx')

#run the cleaning function on our data, adjust as needed
pharmtimes$Company <- as.factor(cleaning(pharmtimes$Company))

#check our cleaning works
levels(pharmtimes$Company)
#write out
write.csv(pharmtimes,file='./data/business_innovation/final/Trade_Journals/Pharmacy_Times.csv',row.names = F)
###pharmacy times bar charts again
max <- table(pharmtimes$Company)
max <- as.data.frame(max)
colnames(max) <- c("Company", "Freq")
pharmtimes <- table(pharmtimes$Company, pharmtimes$Year)
pharmtimes <- as.data.frame(pharmtimes)
colnames(pharmtimes) <- c("Company", "Year", "Freq")
pharmtimes <- subset(pharmtimes,  Freq >= 1)
pharmtimes["max"] = c(0)

for (i in 1:nrow(pharmtimes)){
  for (k in 1:nrow(max)){
    if (pharmtimes$Company[i] == max$Company[k]){
      pharmtimes$max[i] = max$Freq[k]
    }
  }
}
pharmtimes <- pharmtimes[order(-pharmtimes$max),]
pharmtimes <- subset(pharmtimes,  max >= 10)
#change the order with factor
pharmtimes$Year <- factor(pharmtimes$Year, levels = c(2015,2014,2013))
vis <- ggplot(pharmtimes,aes(x= reorder(Company, -as.numeric(max)),y=Freq)) + geom_col(color='black',aes(fill = Year)) +
  scale_y_continuous(breaks = round(seq(0, 35, by = 5))) +
  scale_fill_manual(values=c("#619CFF", "#00BA38", "#F8766D")) +
  theme_bw()+
  theme(title = element_text(size=20), axis.text.x=element_text(size=24, face = 'bold'), axis.text.y = element_text(size = 22, face = 'bold'),
        axis.title.x = element_text(size = 28, face = 'bold'),axis.title.y = element_text(size = 28, face = 'bold'))+
  theme(plot.title = element_text(size = 38, face = 'bold'))+
  theme(legend.text=element_text(size=20),legend.key.size = unit(1.25,'cm'))+
  theme(axis.text.x = element_text(angle = -35, hjust = 0)) + xlab("Company") +
  ylab("Number of News Articles") + ggtitle("Top Companies in Pharmacy Times Product News 2013-2015")
vis

##### repeat on pharmacy today ----
#run the cleaning function on our data, adjust as needed
ptoday$Parent.Company <- as.factor(cleaning(ptoday$Parent.Company))

#check our cleaning works
levels(ptoday$Parent.Company)
write.csv(ptoday,file='./data/business_innovation/final/Trade_Journals/Pharmacy_Today.csv',row.names = F)

###pharmacy times bar charts again
max <- table(ptoday$Parent.Company)
max <- as.data.frame(max)
colnames(max) <- c("Company", "Freq")
ptoday <- table(ptoday$Parent.Company, ptoday$Year)
ptoday <- as.data.frame(ptoday)
colnames(ptoday) <- c("Company", "Year", "Freq")
ptoday <- subset(ptoday,  Freq >= 1)
ptoday["max"] = c(0)

for (i in 1:nrow(ptoday)){
  for (k in 1:nrow(max)){
    if (ptoday$Company[i] == max$Company[k]){
      ptoday$max[i] = max$Freq[k]
    }
  }
}
ptoday <- ptoday[order(-ptoday$max),]
ptoday <- subset(ptoday,  max >= 3)
#change the order with factor
ptoday$Year <- factor(ptoday$Year, levels = c(2015,2014,2013))
vis <- ggplot(ptoday,aes(x= reorder(Company, -as.numeric(max)),y=Freq)) + geom_col(color='black',aes(fill = Year)) +
  scale_y_continuous(breaks = round(seq(0, 10, by = 1))) +
  scale_fill_manual(values=c("#619CFF", "#00BA38", "#F8766D")) +
  theme_bw()+
  theme(title = element_text(size=20), axis.text.x=element_text(size=24, face = 'bold'), axis.text.y = element_text(size = 22, face = 'bold'),
        axis.title.x = element_text(size = 28, face = 'bold'),axis.title.y = element_text(size = 28, face = 'bold'))+
  theme(plot.title = element_text(size = 38, face = 'bold'))+
  theme(legend.text=element_text(size=20),legend.key.size = unit(1.25,'cm'))+
  theme(axis.text.x = element_text(angle = -35, hjust = 0)) + xlab("Company") +
  ylab("Number of News Articles") + ggtitle("Top Companies in Pharmacy Today 2013-2015")
vis


##### Start FDa stuff -----
FDA_Drugs$Year <- sapply(strsplit(as.character(FDA_Drugs$`Approval Date`),"-"), '[', 1)
FDA_Drugs$Company <- as.factor(cleaning(FDA_Drugs$Company))

#check our cleaning works
levels(FDA_Drugs$Company)
write.csv(FDA_Drugs,file='./data/business_innovation/final/FDA_DRUGS/Fda_Drugs.csv',row.names = F)

###pharmacy times bar charts again
max <- table(FDA_Drugs$Company)
max <- as.data.frame(max)
colnames(max) <- c("Company", "max")
FDA_Drugs <- table(FDA_Drugs$Company, FDA_Drugs$Year)
FDA_Drugs <- as.data.frame(FDA_Drugs)
colnames(FDA_Drugs) <- c("Company", "Year", "Freq")
FDA_Drugs <- subset(FDA_Drugs,  Freq >= 1)

FDA_Drugs <- left_join(FDA_Drugs,max,by = "Company")

FDA_Drugs <- FDA_Drugs[order(-FDA_Drugs$max),]
FDA_Drugs <- subset(FDA_Drugs,  max >= 350)
#change the order with factor
FDA_Drugs$Year <- factor(FDA_Drugs$Year, levels = c(2015,2014,2013))
vis <- ggplot(FDA_Drugs,aes(x= reorder(Company, -as.numeric(max)),y=Freq)) + geom_col(color='black',aes(fill = Year)) +
  scale_y_continuous(breaks = round(seq(0, 1000, by = 50))) +
  scale_fill_manual(values=c("#619CFF", "#00BA38", "#F8766D")) +
  theme_bw()+
  theme(title = element_text(size=20), axis.text.x=element_text(size=24, face = 'bold'), axis.text.y = element_text(size = 22, face = 'bold'),
        axis.title.x = element_text(size = 28, face = 'bold'),axis.title.y = element_text(size = 28, face = 'bold'))+
  theme(plot.title = element_text(size = 38, face = 'bold'))+
  theme(legend.text=element_text(size=20),legend.key.size = unit(1.25,'cm'))+
  theme(axis.text.x = element_text(angle = -35, hjust = 0)) + xlab("Company") +
  ylab("Number of Submissions") + ggtitle("Top Companies in FDA Drugs 2013-2015")
vis


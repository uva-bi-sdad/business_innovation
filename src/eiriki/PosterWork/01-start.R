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
  ylab("Number of News Articles") + ggtitle("Top Companies in Pharmacy Times OTC Product News 2013-2015")
vis



##### repeat on pharmacy today ----
co_name <- ptoday$Parent.Company
co_name <- tolower(co_name)
class(co_name)
x <- cleaning(co_name)
ptoday$Company <- x
ptoday$Company <- as.factor(ptoday$Company)
plot1 <- table(ptoday$Company)
plot1 <- as.data.frame(plot1)
colnames(plot1) <- c("Company", "Freq")
plot2 <- subset(plot1, plot1$Freq > 2) #this should get top 10
ggplot(plot2, aes(x=Company, y = Freq)) + geom_col()


##### Start FDa stuff -----
length(unique(FDA_Drugs$Company))
length(unique(cleaning(FDA_Drugs$Company)))


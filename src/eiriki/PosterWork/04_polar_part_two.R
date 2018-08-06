#some polar stuff again
library(ggplot2)
library(dplyr)
library(readxl)
all_data <- read_xlsx('./data/business_innovation/working/tmp_Stuff.xlsx')
#fix levels
all_data$Year <- factor(all_data$Year, levels = c(2015,2014,2013))

emptybar = 10
to_add = matrix(NA,emptybar,ncol(all_data))
colnames(to_add) <- colnames(all_data)
all_data <- rbind(all_data, to_add)
all_data$id <- seq(1,nrow(all_data))

#copied
label_data=all_data
number_of_bar=nrow(label_data)
angle= 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
label_data$hjust<-ifelse( angle < -90, 1, 0)
label_data$angle<-ifelse(angle < -90, angle+180, angle)

all_data$Company <- as.factor(all_data$Company)
vis <- ggplot(label_data,aes(x= reorder(as.factor(id), -as.numeric(label_data$`Total Count`)),y=label_data$Count)) + geom_col(color='black',aes(fill = Year)) +
  scale_y_continuous(breaks = round(seq(0, 35, by = 5))) +
  scale_fill_manual(values=c("#619CFF", "#00BA38", "#F8766D")) +
  theme_bw()+
  theme(title = element_text(size=20), axis.text.x=element_text(size=24, face = 'bold'), axis.text.y = element_text(size = 22, face = 'bold'),
        axis.title.x = element_text(size = 28, face = 'bold'),axis.title.y = element_text(size = 28, face = 'bold'))+
  theme(plot.title = element_text(size = 38, face = 'bold'))+
  theme(legend.text=element_text(size=20),legend.key.size = unit(1.25,'cm'))+
  theme(axis.text.x = element_text(angle = -35, hjust = 0)) + xlab("Company") +
  ylab("Number of News Articles") + ggtitle("Top Companies in Pharmacy Times Product News 2013-2015")

vis + coord_polar(start = 0) +
  ylim(-100,120) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1,4), "cm")
  )








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
pharmtimes2 <- subset(pharmtimes,  max >= 10)
#change the order with factor
pharmtimes$Year <- factor(pharmtimes$Year, levels = c(2015,2014,2013))
vis <- ggplot(pharmtimes2,aes(x= reorder(Company, -as.numeric(max)),y=Freq)) + geom_col(color='black',aes(fill = Year)) +
  scale_y_continuous(breaks = round(seq(0, 35, by = 5))) +
  scale_fill_manual(values=c("#619CFF", "#00BA38", "#F8766D")) +
  theme_bw()+
  theme(title = element_text(size=20), axis.text.x=element_text(size=24, face = 'bold'), axis.text.y = element_text(size = 22, face = 'bold'),
        axis.title.x = element_text(size = 28, face = 'bold'),axis.title.y = element_text(size = 28, face = 'bold'))+
  theme(plot.title = element_text(size = 38, face = 'bold'))+
  theme(legend.text=element_text(size=20),legend.key.size = unit(1.25,'cm'))+
  theme(axis.text.x = element_text(angle = -35, hjust = 0)) + xlab("Company") +
  ylab("Number of News Articles") + ggtitle("Top Companies in Pharmacy Times Product News 2013-2015")

vis + coord_polar() + theme(axis.title.x=element_blank(),
                            axis.text.x=element_blank(),
                            axis.ticks.x=element_blank())


#no subset
pharmtimes <- pharmtimes[order(-pharmtimes$max),]
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

vis + coord_polar() + theme(axis.title.x=element_blank(),
                            axis.text.x=element_blank(),
                            axis.ticks.x=element_blank())

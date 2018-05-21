#this script is going back to the Pharmacy Times data and doing new visuals for 2013-2017
#we have already done some for 2015 and 2017
library(dplyr)
library(plyr)
library(stringr)
library(ggplot2)

#pull in all the data from OTC Pharmacy times
file_list <- list.files("./data/business_innovation/working/PHARMACY_TIMES/Rx/", pattern="*.txt")
data = data.frame("temp" = c(0))

for (i in 1:length(file_list)){
  temp <- readLines(paste0("./data/business_innovation/working/PHARMACY_TIMES/Rx/",file_list[i]), encoding = "UTF-8")
  temp <- data.frame(temp)
  data = rbind(data,temp)
}
data = data[-1,] #get rid of the fist row, it will always be empty

#set up "empty" data frame
v = data.frame("Drug Name" = c(1:length(data)), "Company" = c(1:length(data)), "Date" = c(1:length(data)),
               "Year" = c(1:length(data)))
#fill frame with NA
for(i in 1:length(data)){
  v$Drug.Name[i] = NA
  v$Company[i] = NA
  v$Date[i] = NA
  v$Year[i] = NA
}
date = "0" #need an initial value for date and year
year = "0"
first = FALSE
#this block is sacred#################################
for(i in 1:length(data)){
  #whenever we see "Date:", we want to reset variable date to know it's a new article
  if (str_detect(data[i], 'Date: ')){
    date = str_extract(data[i], '(?<=(Date: )).*')
    year = str_extract(date, '(?<=(, )).*')
    #this bool handles the first drug in 2015 and 2016 articles, they appear right below the date
    #first = TRUE
    next() #this goes to the next iteration of the for loop
  }
  if (str_detect(str_to_lower(data[i]), 'marketed|manufactured')){
    v$Drug.Name[i] = str_extract(data[i-1], '.*')
    v$Company[i] = str_extract(data[i+1], '.*')
  }
  v$Date[i] = date
  v$Year[i] = year
}
#This BLOCK IS SACRED###################################
v <- v[complete.cases(v),] #this gets rid of any row with NA's. Consider replacing loop above once done
v <- na.omit(v)
for (i in 1:nrow(v)){
  if (is.na(v$Company[i]) | v$Company[i] == ""){
    v <- v[-i,]
  }
}
v$Company <- as.factor(v$Company)
v$Date <- as.Date(v$Date, format = "%B %d, %Y")
v$Year <- as.numeric(v$Year)
#write.csv(v, file = "./data/business_innovation/working/PHARMACY_TIMES/Rx/Rx_dirty.csv")
#graphics - ####################################################MOVE TO ANOTHER FILE
# pharmTimes <- table(v$Company)
# pharmTimes <- as.data.frame(pharmTimes)
# colnames(pharmTimes) <- c("Company", "Freq")
# #sort only top companies of 2015
# pharmTimes <- subset(pharmTimes, Freq >= 4)
# #ggplot2
# vis <- ggplot(pharmTimes,aes(x= reorder(Company, -as.numeric(Freq)), y= Freq)) + geom_bar(stat= "identity") +
#   scale_y_continuous(breaks = round(seq(0, max(pharmTimes$Freq), by = 1))) +
#   theme_bw()+
#   theme(title = element_text(size=20), axis.text.x=element_text(size=16, face = 'bold'), axis.text.y = element_text(size = 15, face = 'bold'),
#         axis.title.x = element_text(size = 17, face = 'bold'),axis.title.y = element_text(size = 17, face = 'bold'))+
#   theme(plot.title = element_text(size = 28, face = 'bold'))+
#   theme(axis.text.x = element_text(angle = 40, hjust = 1)) + xlab("Company") +
#   ylab("Times Feautured in News Article") + ggtitle("Top Companies in Pharmacy Times OTC Product News 2015-2017")
# vis
# #ggsave(vis, filename = "otc_2017.png",width=20,height=11.25,scale=1,path = "./src/eiriki/PharmacyTimes/")
#
# pfizertable = subset(v, v$Company == "Pfizer Consumer Healthcare")
# vis2 <- ggplot(pfizertable, aes(x=Date,y=Year)) + geom_histogram(stat= "identity") +
#   theme_bw()+
#   theme(title = element_text(size=20), axis.text.x=element_text(size=16, face = 'bold'), axis.text.y = element_text(size = 15, face = 'bold'),
#         axis.title.x = element_text(size = 17, face = 'bold'),axis.title.y = element_text(size = 17, face = 'bold'))+
#   theme(plot.title = element_text(size = 28, face = 'bold'))+
#   theme(axis.text.x = element_text(angle = 40, hjust = 1)) + xlab("Company") +
#   ylab("Times Feautured in News Article") + ggtitle("Pfizer OTC Releases 2015-2017")
# vis2

#stacked bar
pharmTimes <- subset(v, (Year >= 2013))
pharmTimes <- subset(pharmTimes, (Year < 2016))


#fixing spacing and punctuation issues
#take out Pharma and other filler words
pharmTimes$Company <- str_replace_all(pharmTimes$Company, ": ", "")
pharmTimes$Company <- str_replace_all(pharmTimes$Company, ", Inc", "")
pharmTimes$Company <- str_replace_all(pharmTimes$Company, " Inc", "")
pharmTimes$Company <- str_replace_all(pharmTimes$Company, "America", "")
pharmTimes$Company <- str_replace_all(pharmTimes$Company, "Pharmaceuticals", "")
pharmTimes$Company <- str_replace_all(pharmTimes$Company, "Pharma", "")
pharmTimes$Company <- str_replace_all(pharmTimes$Company, "(?<=^\\ )", "")
pharmTimes$Company <- as.factor(pharmTimes$Company)

max <- table(pharmTimes$Company)
max <- as.data.frame(max)
colnames(max) <- c("Company", "Freq")
pharmTimes <- table(pharmTimes$Company, pharmTimes$Year)
pharmTimes <- as.data.frame(pharmTimes)
colnames(pharmTimes) <- c("Company", "Year", "Freq")
pharmTimes <- subset(pharmTimes,  Freq >= 1)
pharmTimes["max"] = c(0)

for (i in 1:nrow(pharmTimes)){
  for (k in 1:nrow(max)){
    if (pharmTimes$Company[i] == max$Company[k]){
      pharmTimes$max[i] = max$Freq[k]
    }
  }
}
pharmTimes <- subset(pharmTimes,  max >= 2)
#change the order with factor
pharmTimes$Year <- factor(pharmTimes$Year, levels = c(2015,2014,2013))
pharmTimes <- arrange(pharmTimes,-as.numeric(Year))
vis <- ggplot(pharmTimes,aes(x= reorder(Company, -as.numeric(max)),y=Freq, fill = Year)) + geom_col(color = 'black',position='stack') +
  scale_y_continuous(breaks = round(seq(0, 15, by = 1))) +
  scale_fill_manual(values=c("#619CFF", "#00BA38", "#F8766D")) +
  theme_bw()+
  theme(title = element_text(size=20), axis.text.x=element_text(size=24, face = 'bold'), axis.text.y = element_text(size = 22, face = 'bold'),
        axis.title.x = element_text(size = 28, face = 'bold'),axis.title.y = element_text(size = 28, face = 'bold'))+
  theme(plot.title = element_text(size = 38, face = 'bold'))+
  theme(legend.text=element_text(size=20),legend.key.size = unit(1.25,'cm'))+
  theme(axis.text.x = element_text(angle = -35, hjust = 0)) + xlab("Company") +
  ylab("Number of News Articles") + ggtitle("Top Companies in Pharmacy Times Rx Product News 2013-2015")
vis

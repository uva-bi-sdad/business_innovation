#this script is going back to the Pharmacy Times data and doing new visuals for 2013-2017
#we have already done some for 2015 and 2017
library(dplyr)
library(plyr)
library(stringr)
library(ggplot2)

#pull in all the data from OTC Pharmacy times
file_list <- list.files("./data/business_innovation/working/PHARMACY_TIMES/OTC/", pattern="*.txt")
data = data.frame("temp" = c(0))

for (i in 1:length(file_list)){
  temp <- readLines(paste0("./data/business_innovation/working/PHARMACY_TIMES/OTC/",file_list[i]), encoding = "UTF-8")
  temp <- data.frame(temp)
  data = rbind(data,temp)
}
data = data[-1,] #get rid of the fist row

#this block is sacred#################################
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
for(i in 1:length(data)){
  #whenever we see "Date:", we want to reset variable date to know it's a new article
  if (str_detect(data[i], 'Date: ')){
    date = str_extract(data[i], '(?<=(Date: )).*')
    year = str_extract(date, '(?<=(, )).*')
    #this if statement handles the first drug in 2015 articles, they appear right below the date
    if(year == "2015" | year == "2016"){
      v$Drug.Name[i] <- str_extract(data[i+1], '^.*')
      v$Company[i] <- str_extract(data[i+2], '(?<=By: |by: |BY: )(?s)((?:\\S+\\s+){0,3}\\S+)')
      v$Date[i] = date
      v$Year[i] = year
      next() #this goes to the next iteration of the for loop
    }
  }
  #when making a data table, we extract information according to patterns in data storage.
  #since this text data is stored with spurious patterns, we do it based on year since each year seems to have
  #somewhat of a storage pattern.
  if(year == "2015" | year == "2016"){ #noted bugs for 2015: please check 'products'
    v$Drug.Name[i] <- str_extract(data[i], '(?<=(\\.com)).*')
    v$Drug.Name[i] <- str_extract(v$Drug.Name[i], '[^a-z\\-\\/]+([A-Z]*[A-z\\’\\-\\&0-9_\\s]*)*')
    v$Company[i] <- str_extract(data[i+1], '(?<=By: |by: |BY: )(?s)((?:\\S+\\s+){0,3}\\S+)')
    v$Date[i] = date
    v$Year[i] = year
  }
  else if(year == "2017"){
    v$Drug.Name[i] <- str_extract(data[i], '^.*(?=Marketed | MARKETED)')
    v$Company[i] <- str_extract(data[i], '(?<=By: |by: |BY: )(?s)((?:\\S+\\s+){0,3}\\S+)')
    v$Date[i] = date
    v$Year[i] = year
    #if our text is formatted strange and drug name return NA, the name is usually pinned between 'Marketed By' and '.com'
    if (is.na(v$Drug.Name[i])){
      v$Drug.Name[i] <- str_extract(data[i], '(?<=(\\.com)).*')
      v$Drug.Name[i] <- str_extract(v$Drug.Name[i], '[^a-z\\-\\/]+([A-Z]*[A-z\\’\\-\\&0-9_\\s]*)*')
      v$Company[i] <- str_extract(data[i], '(?<=By: |by: |BY: )(?s)((?:\\S+\\s+){0,3}\\S+)')
      v$Date[i] = date
      v$Year[i] = year
    }
  }
  # v$Drug.Name[i] <- str_extract(data[i], '^.*(?=Marketed | MARKETED)')
  # v$Company[i] <- str_extract(data[i], '(?<=By: |by: |BY: )(?s)((?:\\S+\\s+){0,3}\\S+)')
  # v$Date[i] = date
  # #if our text is formatted strange and drug name return NA, the name is usually pinned between 'Marketed By' and '.com'
  # if (is.na(v$Drug.Name[i])){
  #   v$Drug.Name[i] <- str_extract(data[i], '(?<=(com))(.*?)(?=Marketed | MARKETED)')
  #   v$Company[i] <- str_extract(data[i], '(?<=By: |by: |BY: )(?s)((?:\\S+\\s+){0,3}\\S+)')
  #   v$Date[i] = date
  # }
}
#This BLOCK IS SACRED###################################
for(i in 1:length(data)){
  if(is.na(v$Drug.Name[i]) && is.na(v$Company[i])){
    v = v[-i,]
  }
}
v$Company <- as.factor(v$Company)
v$Date <- as.Date(v$Date, format = "%B %d, %Y")
v$Year <- as.numeric(v$Year)

#graphics
pharmTimes <- table(v$Company)
pharmTimes <- as.data.frame(pharmTimes)
colnames(pharmTimes) <- c("Company", "Freq")
#sort only top companies of 2015
pharmTimes <- subset(pharmTimes, Freq >= 4)
#ggplot2
vis <- ggplot(pharmTimes,aes(x= reorder(Company, -as.numeric(Freq)), y= Freq)) + geom_bar(stat= "identity") +
  scale_y_continuous(breaks = round(seq(0, max(pharmTimes$Freq), by = 1))) +
  theme_bw()+
  theme(title = element_text(size=20), axis.text.x=element_text(size=16, face = 'bold'), axis.text.y = element_text(size = 15, face = 'bold'),
        axis.title.x = element_text(size = 17, face = 'bold'),axis.title.y = element_text(size = 17, face = 'bold'))+
  theme(plot.title = element_text(size = 28, face = 'bold'))+
  theme(axis.text.x = element_text(angle = 40, hjust = 1)) + xlab("Company") +
  ylab("Times Feautured in News Article") + ggtitle("Top Companies in Pharmacy Times OTC Product News 2015-2017")
vis
#ggsave(vis, filename = "otc_2017.png",width=20,height=11.25,scale=1,path = "./src/eiriki/PharmacyTimes/")

pfizertable = subset(v, v$Company == "Pfizer Consumer Healthcare")
vis2 <- ggplot(pfizertable, aes(x=Date,y=Year)) + geom_histogram(stat= "identity") +
  theme_bw()+
  theme(title = element_text(size=20), axis.text.x=element_text(size=16, face = 'bold'), axis.text.y = element_text(size = 15, face = 'bold'),
        axis.title.x = element_text(size = 17, face = 'bold'),axis.title.y = element_text(size = 17, face = 'bold'))+
  theme(plot.title = element_text(size = 28, face = 'bold'))+
  theme(axis.text.x = element_text(angle = 40, hjust = 1)) + xlab("Company") +
  ylab("Times Feautured in News Article") + ggtitle("Pfizer OTC Releases 2015-2017")
vis2

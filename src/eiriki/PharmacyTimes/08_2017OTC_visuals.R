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
    if(nchar(v$Company[i]) >= 60){
      v$Company[i] = str_extract(data[i+1], '([^\\s]+)')
    }
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
# #this block is the old way we analyzed
# for(i in 1:length(data)){
#   #whenever we see "Date:", we want to reset variable date to know it's a new article
#   if (str_detect(data[i], 'Date: ')){
#     date = str_extract(data[i], '(?<=(Date: )).*')
#     year = str_extract(date, '(?<=(, )).*')
#     #this bool handles the first drug in 2015 and 2016 articles, they appear right below the date
#     first = TRUE
#     next() #this goes to the next iteration of the for loop
#   }
#   #when making a data table, we extract information according to patterns in data storage.
#   #since this text data is stored with various patterns, we do it based on year since each year seems to have
#   #somewhat of a storage pattern.
#   if(date == "JANUARY 09, 2015"){ #all special cases for the first script
#     #if right after a date, perform a certain pattern.
#     if(first == TRUE){
#       v$Drug.Name[i] <- str_extract(data[i], '.*(?=(Marketed|marketed))')
#       v$Company[i] <- str_extract(data[i], '(?<=(\\by: |\\BY: |\\By: )).*')
#       v$Date[i] = date
#       v$Year[i] = year
#       first = FALSE
#       next()
#     }
#      first_reg <- str_extract(data[i], '(?<=(\\.com)).*')
#      v$Drug.Name[i] <- str_extract(first_reg, '[^a-z\\-\\/]+([A-Z]*[A-z\\’\\-\\&0-9_\\s]*)*')
#      v$Company[i] <- str_extract(data[i+1], '(?<=By: |by: |BY: )(?s)((?:\\S+\\s+){0,3}\\S+)')
#       #case for when there are multiple '.com's in a product description
#       if(is.na(v$Drug.Name[i]) || nchar(v$Drug.Name[i]) >= 40){
#         first_reg <- str_extract(data[i], '(?<=(\\www.)).*')
#         v$Drug.Name[i] <- str_extract(first_reg, '(?<=(\\.com)).*')
#       }
#       #case for the second product after date: See January
#       if(is.na(v$Drug.Name[i])){
#        v$Drug.Name[i] <- str_extract(data[i], '^.*(?=Marketed|MARKETED)')
#       }
#       #case for if the company is on the same line as the product name
#       if(is.na(v$Company[i])){
#         v$Company[i] <- str_extract(data[i], '(?<=By: |by: |BY: )(?s)((?:\\S+\\s+){0,3}\\S+)')
#       }
#   }
#   if(date == "DECEMBER 05, 2016"){ #special cases for the last in 2016
#     #if right after a date, perform a certain pattern.
#     if(first == TRUE){
#       v$Drug.Name[i] <- str_extract(data[i], '.*(?=(Marketed|marketed))')
#       v$Company[i] <- str_extract(data[i], '(?<=(\\by:|\\BY:|\\By:)).*')
#       v$Date[i] = date
#       v$Year[i] = year
#       first = FALSE
#       next()
#     }
#     v$Drug.Name[i] <- str_extract(data[i], '.*(?=(Marketed|marketed))')
#     v$Company[i] <- str_extract(data[i], '(?<=(\\by:|\\BY:|\\By:)).*')
#   }
#
#   else if(year == "2015"){ #all cases for the rest of 2015
#     #if right after a date, perform a certain pattern.
#     if(first == TRUE){
#       v$Drug.Name[i] <- str_extract(data[i], '.*')
#       v$Company[i] <- str_extract(data[i+1], '(?<=(\\by: |\\BY: |\\By: )).*')
#       v$Date[i] = date
#       v$Year[i] = year
#       first = FALSE
#       next()
#     }
#     first_reg <- str_extract(data[i], '(?<=(\\www.)).*')
#     first_reg<- str_extract(first_reg, '(?<=(\\.com)).*')
#     v$Drug.Name[i] <- str_extract(first_reg, '[^a-z\\-\\/]+([A-Z]*[A-z\\’\\-\\&0-9_\\s]*)')
#     if(is.na(v$Drug.Name[i])){
#       v$Drug.Name[i] <- first_reg
#     }
#     v$Company[i] <- str_extract(data[i+1], '(?<=By: |by: |BY: )(?s)((?:\\S+\\s+){0,3}\\S+)')
#   }
#   else if(year == "2016" || year == "2017"){ #all cases for 2016
#     #if right after a date, perform a certain pattern.
#     if(first == TRUE){
#       v$Drug.Name[i] <- str_extract(data[i], '.*')
#       v$Company[i] <- str_extract(data[i+1], '(?<=(\\by: |\\BY: |\\By: )).*')
#       v$Date[i] = date
#       v$Year[i] = year
#       first = FALSE
#       next()
#     }
#     first_reg<- str_extract(data[i], '(?<=(\\.com)).*')
#     v$Drug.Name[i] <- first_reg
#     # v$Drug.Name[i] <- str_extract(first_reg, '[^a-z\\-\\/]+([A-Z]*[A-z\\’\\-\\&0-9_\\s]*)')
#     # if(is.na(v$Drug.Name[i])){
#     #   v$Drug.Name[i] <- first_reg
#     # }
#     v$Company[i] <- str_extract(data[i+1], '(?<=By: |by: |BY: )(?s)((?:\\S+\\s+){0,3}\\S+)')
#   }
#   else if(year == "2014" || year == "2013"){ #all cases for 2014 and 2013
#     #if right after a date, perform a certain pattern.
#     if(first == TRUE){
#       v$Drug.Name[i] <- str_extract(data[i], '^.*(?=Marketed | MARKETED)')
#       v$Company[i] <- str_extract(data[i], '(?<=(\\by: |\\BY: |\\By: )).*')
#       v$Date[i] = date
#       v$Year[i] = year
#       first = FALSE
#       next()
#     }
#     first_reg<- str_extract(data[i], '(?<=(\\.com)).*')
#     if (grepl("(Marketed|MARKETED|Manufactured)",first_reg)){
#       first_reg <- str_extract(data[i], '^.*(?=Marketed|MARKETED|Manufactured)')
#       v$Drug.Name[i] <- first_reg
#       v$Company[i] <- str_extract(data[i], '(?<=(\\by: |\\BY: |\\By: )).*')
#       next()
#     }
#     v$Drug.Name[i] <- first_reg
#     # v$Drug.Name[i] <- str_extract(first_reg, '[^a-z\\-\\/]+([A-Z]*[A-z\\’\\-\\&0-9_\\s]*)')
#     # if(is.na(v$Drug.Name[i])){
#     #   v$Drug.Name[i] <- first_reg
#     # }
#
#     v$Company[i] <- str_extract(data[i+1], '(?<=By: |by: |BY: )(?s)((?:\\S+\\s+){0,3}\\S+)')
#   }
#
#   # else if(year == "2017"){
#   #   v$Drug.Name[i] <- str_extract(data[i], '^.*(?=Marketed | MARKETED)')
#   #   v$Company[i] <- str_extract(data[i], '(?<=By: |by: |BY: )(?s)((?:\\S+\\s+){0,3}\\S+)')
#   #   v$Date[i] = date
#   #   v$Year[i] = year
#   #   #if our text is formatted strange and drug name return NA, the name is usually pinned between 'Marketed By' and '.com'
#   #   if (is.na(v$Drug.Name[i])){
#   #     v$Drug.Name[i] <- str_extract(data[i], '(?<=(\\.com)).*')
#   #     v$Drug.Name[i] <- str_extract(v$Drug.Name[i], '[^a-z\\-\\/]+([A-Z]*[A-z\\’\\-\\&0-9_\\s]*)*')
#   #     v$Company[i] <- str_extract(data[i], '(?<=By: |by: |BY: )(?s)((?:\\S+\\s+){0,3}\\S+)')
#
#   #   }
#   # }
#   # v$Drug.Name[i] <- str_extract(data[i], '^.*(?=Marketed | MARKETED)')
#   # v$Company[i] <- str_extract(data[i], '(?<=By: |by: |BY: )(?s)((?:\\S+\\s+){0,3}\\S+)')
#   # v$Date[i] = date
#   # #if our text is formatted strange and drug name return NA, the name is usually pinned between 'Marketed By' and '.com'
#   # if (is.na(v$Drug.Name[i])){
#   #   v$Drug.Name[i] <- str_extract(data[i], '(?<=(com))(.*?)(?=Marketed | MARKETED)')
#   #   v$Company[i] <- str_extract(data[i], '(?<=By: |by: |BY: )(?s)((?:\\S+\\s+){0,3}\\S+)')
#   #
#   # }
#   v$Date[i] = date
#   v$Year[i] = year
# }
# #This BLOCK IS the old way we analyzed
v <- v[complete.cases(v),] #this gets rid of any row with NA's. Consider replacing loop above once done
v <- na.omit(v)
v$Company <- as.factor(v$Company)
v$Date <- as.Date(v$Date, format = "%B %d, %Y")
v$Year <- as.numeric(v$Year)
#write.csv(v, file = "./data/business_innovation/working/PHARMACY_TIMES/OTC/otc_dirty.csv")

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
pharmTimes$Company <- str_replace_all(pharmTimes$Company, ", LLC", "")
pharmTimes$Company <- str_replace_all(pharmTimes$Company, " LLC", "")
pharmTimes$Company <- str_replace_all(pharmTimes$Company, "Holdings", "")
pharmTimes$Company <- str_replace_all(pharmTimes$Company, "(?<=(\\Chattem)).*", "")
pharmTimes$Company <- str_replace_all(pharmTimes$Company, "Pharmaceuticals", "")
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
pharmTimes <- pharmTimes[order(-pharmTimes$max),]
pharmTimes <- subset(pharmTimes,  max >= 3)
#change the order with factor
pharmTimes$Year <- factor(pharmTimes$Year, levels = c(2015,2014,2013))
vis <- ggplot(pharmTimes,aes(x= reorder(Company, -as.numeric(max)),y=Freq)) + geom_col(color='black',aes(fill = Year)) +
  scale_y_continuous(breaks = round(seq(0, 18, by = 1))) +
  scale_fill_manual(values=c("#619CFF", "#00BA38", "#F8766D")) +
  theme_bw()+
  theme(title = element_text(size=20), axis.text.x=element_text(size=24, face = 'bold'), axis.text.y = element_text(size = 22, face = 'bold'),
        axis.title.x = element_text(size = 28, face = 'bold'),axis.title.y = element_text(size = 28, face = 'bold'))+
  theme(plot.title = element_text(size = 38, face = 'bold'))+
  theme(legend.text=element_text(size=20),legend.key.size = unit(1.25,'cm'))+
  theme(axis.text.x = element_text(angle = -35, hjust = 0)) + xlab("Company") +
  ylab("Number of News Articles") + ggtitle("Top Companies in Pharmacy Times OTC Product News 2013-2015")
vis

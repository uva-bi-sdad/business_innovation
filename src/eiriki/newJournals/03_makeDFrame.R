#Now that we have all of the text data scraped, we must put it into a data frame to read easily
library(dplyr)
library(plyr)
library(stringr)
library(ggplot2)

#pull in all the data from OTC Pharmacy times
file_list <- list.files("./data/business_innovation/working/PHARMACY_TODAY/", pattern="*.txt")
data = data.frame("temp" = c(0))

for (i in 1:length(file_list)){
  temp <- readLines(paste0("./data/business_innovation/working/PHARMACY_TODAY/",file_list[i]), encoding = "UTF-8")
  temp <- data.frame(temp)
  data = rbind(data,temp)
}
data = data[-1,] #get rid of the fist row, it will always be empty

#set up "empty" data frame
v = data.frame("Drug Name" = c(1:length(data)), "Company" = c(1:length(data)),
               "Parent Company" = c(1:length(data)), "Release Type" = c(1:length(data)),
               "Date" = c(1:length(data)),"Year" = c(1:length(data)))
#fill frame with NA
v$Drug.Name = NA
v$Company = NA
v$Parent.Company = NA
v$Release.Type = NA
v$Date= NA
v$Year = NA

date = "0" #need an initial value for date and year
year = "0"
rel <- "0"
######################START FILLING DATA Frame################
for(i in 1:length(data)){
  #whenever we see "Date:", we want to reset variable date to know it's a new article
  if (str_detect(data[i], 'Date: ')){
    date = str_extract(data[i], '(?<=(Date: )).*')
    year = str_extract(date, '([[:digit:]]).*')
    #this bool handles the first drug in 2015 and 2016 articles, they appear right below the date
    first = TRUE
    rel <- "New Approvals"
    next() #this goes to the next iteration of the for loop
  }
  #whenever we know the category is changing, reset variable release type
  if (str_detect(str_to_lower(data[i]), 'new approval|new formulation|new indication')){
    rel <- data[i]
    next() #this goes to the next iteration of the for loop
  }
  v$Drug.Name[i] <- str_extract(data[i], '^.*(?=\\()') #everything before parentheses
  v$Company[i] <- str_extract(data[i], '(?<=\\().*(?=\\))')  #everything inbetween parentheses
  v$Parent.Company[i] <- str_extract(data[i], '(?<=\\().*(?=\\))')  #everything inbetween parentheses
  #throw away trash
  if(!is.na(v$Company) && (nchar(v$Company[i]) > 40 || v$Company[i] == 'human')){
    v$Drug.Name[i] <- NA
    v$Company[i] <- NA
    v$Parent.Company[i] <- NA
  }
  #check if we have a parent company
  if(str_detect(data[i], '—')){
    temp <- v$Company[i]
    v$Company[i] <- str_extract(temp, '.*(?=\\—)')  #everything inbetween parentheses
    v$Parent.Company[i] <- str_extract(temp, '(?<=\\—).*')  #everything inbetween parentheses
  }
  v$Date[i] = date
  v$Year[i] = year
  v$Release.Type[i] = str_to_lower(rel)
}
######################END FILLING DATA Frame##################
i = 1
while(i < nrow(v)){
  if(is.na(v$Drug.Name[i]) ||is.na(v$Company[i])||v$Drug.Name[i] == ""){
    v <- v[-i,]
    i = i-1
  }
  i = i +1
}
#v <- v[complete.cases(v),] #this gets rid of any row with NA's. Consider replacing loop above once done
#v <- na.omit(v)
v$Company <- as.factor(v$Company)
v$Parent.Company <- as.factor(v$Parent.Company)
#v$Date <- as.Date(v$Date, format = "%B %Y")
v$Year <- as.numeric(v$Year)

#stacked bar
pharmTimes <- subset(v, (Year >= 2013))
pharmTimes <- subset(pharmTimes, (Year < 2016))
rownames(pharmTimes) <- seq(length=nrow(pharmTimes))

#cut the noise out by row number
pharmTimes <- pharmTimes[-2,]
pharmTimes <- pharmTimes[-14,]
pharmTimes <- pharmTimes[-20,]
pharmTimes <- pharmTimes[-32,]
pharmTimes <- pharmTimes[-74,]
pharmTimes <- pharmTimes[-90,]
pharmTimes <- pharmTimes[-113,]
#fixing spacing and punctuation issues
#take out Pharma and other filler words
# pharmTimes$Company <- str_replace_all(pharmTimes$Company, ": ", "")
# pharmTimes$Company <- str_replace_all(pharmTimes$Company, ", Inc", "")
# pharmTimes$Company <- str_replace_all(pharmTimes$Company, " Inc", "")
pharmTimes$Parent.Company <- str_replace_all(pharmTimes$Parent.Company, "Pharmaceuticals", "")
# pharmTimes$Company <- str_replace_all(pharmTimes$Company, "Pharma", "")
# pharmTimes$Company <- str_replace_all(pharmTimes$Company, "(?<=^\\ )", "")
pharmTimes$Parent.Company <- as.factor(pharmTimes$Parent.Company)
#export this data set, then bring it back in for analysis after cleaning
#write.csv(pharmTimes, file = "./data/business_innovation/working/PHARMACY_TODAY/data_dirty")


max <- table(pharmTimes$Parent.Company)
max <- as.data.frame(max)
colnames(max) <- c("Company", "Freq")
pharmTimes <- table(pharmTimes$Parent.Company, pharmTimes$Year)
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
vis <- ggplot(pharmTimes,aes(x= reorder(Company, -as.numeric(max)),y=Freq, fill = Year)) + geom_col(color='black',position='stack') +
  scale_y_continuous(breaks = round(seq(0, 15, by = 1))) +
  scale_fill_manual(values=c("#619CFF", "#00BA38", "#F8766D")) +
  theme_bw()+
  theme(title = element_text(size=20), axis.text.x=element_text(size=24, face = 'bold'), axis.text.y = element_text(size = 22, face = 'bold'),
        axis.title.x = element_text(size = 28, face = 'bold'),axis.title.y = element_text(size = 28, face = 'bold'))+
  theme(plot.title = element_text(size = 38, face = 'bold'))+
  theme(legend.text=element_text(size=20),legend.key.size = unit(1.25,'cm'))+
  theme(axis.text.x = element_text(angle = -35, hjust = 0)) + xlab("Company") +
  ylab("Number of News Articles") + ggtitle("Top Companies in Pharmacy Today 2013-2015")
vis

#interested companies visuals
compData <- subset(v, (Year >= 2013))
compData <- subset(compData, (Year < 2016))
#write.csv(compData, file = "./data/business_innovation/working/PHARMACY_TODAY/pToday.csv")

compData$Parent.Company <- str_replace_all(compData$Parent.Company, ": ", "")
compData$Parent.Company <- str_replace_all(compData$Parent.Company, "(?<=(Baxter)).*", "")
compData$Parent.Company <- str_replace_all(compData$Parent.Company, "(?<=(Merck)).*", "")
compData$Parent.Company <- str_replace_all(compData$Parent.Company, "(?<=(Novartis)).*", "")
compData$Parent.Company <- str_replace_all(compData$Parent.Company, "(?<=(Pfizer)).*", "")
compData$Parent.Company <- str_replace_all(compData$Parent.Company, "(?<=(GlaxoSmithKline)).*", "")
compData$Parent.Company <- as.factor(compData$Parent.Company)

spreads <- compData[compData$Parent.Company == "Merck",]
spreads <- rbind(spreads,compData[compData$Parent.Company == "Novartis",])
spreads <- rbind(spreads,compData[compData$Parent.Company == "Baxter",])
spreads <- rbind(spreads,compData[compData$Parent.Company == "Pfizer",])
spreads <- rbind(spreads,compData[compData$Parent.Company == "GlaxoSmithKline",])
spreads$Company <- as.character(spreads$Company)
spreads$Parent.Company <- as.character(spreads$Parent.Company)
#export csv for comparisons
rownames(spreads) <- seq(length=nrow(spreads))
#fix some data
spreads$Drug.Name[4] = "porcine"
spreads$Company[4] = "Obizur"
spreads$Parent.Company <- factor(spreads$Parent.Company)
#write.csv(spreads, file = "./data/business_innovation/working/PHARMACY_TODAY/pTodayInterest.csv")

max <- table(spreads$Parent.Company)
max <- as.data.frame(max)
colnames(max) <- c("Company", "Freq")
spreads <- table(spreads$Parent.Company, spreads$Year)
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
   xlab("Company") +
  ylab("Number of News Articles") + ggtitle("Companies of Interest in Pharmacy Today 2013-2015")
vis

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
               "Date" = c(1:length(data)),"Year" = c(1:length(data)))
#fill frame with NA
v$Drug.Name = NA
v$Company = NA
v$Date= NA
v$Year = NA

date = "0" #need an initial value for date and year
year = "0"

######################START FILLING DATA Frame################
for(i in 1:length(data)){
  #whenever we see "Date:", we want to reset variable date to know it's a new article
  if (str_detect(data[i], 'Date: ')){
    date = str_extract(data[i], '(?<=(Date: )).*')
    year = str_extract(date, '([[:digit:]]).*')
    #this bool handles the first drug in 2015 and 2016 articles, they appear right below the date
    first = TRUE
    next() #this goes to the next iteration of the for loop
  }
  v$Drug.Name[i] <- str_extract(data[i], '^.*(?=\\()') #everything before parentheses
  v$Company[i] <- str_extract(data[i], '(?<=\\().*(?=\\))')  #everything inbetween parentheses
  v$Date[i] = date
  v$Year[i] = year
}
######################END FILLING DATA Frame##################

#v <- v[complete.cases(v),] #this gets rid of any row with NA's. Consider replacing loop above once done
v <- na.omit(v)
v$Company <- as.factor(v$Company)
#v$Date <- as.Date(v$Date, format = "%B %Y")
v$Year <- as.numeric(v$Year)
tab <- table(v$Company)

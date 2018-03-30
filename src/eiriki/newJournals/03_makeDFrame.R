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
v = data.frame("Drug Name" = c(1:length(data)), "Company" = c(1:length(data)), "Release Type" = c(1:length(data)),
               "Date" = c(1:length(data)),"Year" = c(1:length(data)))
#fill frame with NA
v$Drug.Name = NA
v$Company = NA
v$Date= NA
v$Release.Type = NA
v$Year = NA

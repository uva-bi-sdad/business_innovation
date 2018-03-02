#this script is going back to the Pharmacy Times data and doing new visuals for 2013-2017
#we have already done some for 2015 and 2017
library(dplyr)
library(stringr)
library(ggplot2)
#string we are trying to match
market <- "Marketed"
cap <-"MARKETED"

#pull in all the data for 2017
pdat1 <- readLines("./data/business_innovation/working/PHARMACY_TIMES/OTC/14PTscrape_JANUARY 16, 2017.txt", encoding = "UTF-8")
pdat2 <- readLines("./data/business_innovation/working/PHARMACY_TIMES/OTC/13PTscrape_FEBRUARY 20, 2017.txt", encoding = "UTF-8")
pdat3 <- readLines("./data/business_innovation/working/PHARMACY_TIMES/OTC/12PTscrape_MARCH 20, 2017.txt", encoding = "UTF-8")
pdat4 <- readLines("./data/business_innovation/working/PHARMACY_TIMES/OTC/11PTscrape_APRIL 20, 2017.txt", encoding = "UTF-8")
pdat5 <- readLines("./data/business_innovation/working/PHARMACY_TIMES/OTC/10PTscrape_MAY 05, 2017.txt", encoding = "UTF-8")
pdat6 <- readLines("./data/business_innovation/working/PHARMACY_TIMES/OTC/9PTscrape_JUNE 05, 2017.txt", encoding = "UTF-8")
pdat7 <- readLines("./data/business_innovation/working/PHARMACY_TIMES/OTC/8PTscrape_JULY 26, 2017.txt", encoding = "UTF-8")
pdat8 <- readLines("./data/business_innovation/working/PHARMACY_TIMES/OTC/7PTscrape_SEPTEMBER 04, 2017.txt", encoding = "UTF-8")
pdat9 <- readLines("./data/business_innovation/working/PHARMACY_TIMES/OTC/6PTscrape_SEPTEMBER 25, 2017.txt", encoding = "UTF-8")
pdat10 <- readLines("./data/business_innovation/working/PHARMACY_TIMES/OTC/5PTscrape_OCTOBER 27, 2017.txt", encoding = "UTF-8")
pdat11 <- readLines("./data/business_innovation/working/PHARMACY_TIMES/OTC/4PTscrape_NOVEMBER 16, 2017.txt", encoding = "UTF-8")
pdat12 <- readLines("./data/business_innovation/working/PHARMACY_TIMES/OTC/3PTscrape_DECEMBER 20, 2017.txt", encoding = "UTF-8")

#store all the text containing "Marketed" in separate lists: this gives us all company names
pdat01 <- data.frame("Company" = str_subset(pdat1,market))
pdat02 <- data.frame("Company" = str_subset(pdat2,market))
pdat03 <- data.frame("Company" = str_subset(pdat3,market))
pdat04 <- data.frame("Company" = str_subset(pdat4,market))
pdat05 <- data.frame("Company" = str_subset(pdat5,market))
pdat06 <- data.frame("Company" = str_subset(pdat6,market))
pdat07 <- data.frame("Company" = str_subset(pdat7,market))
pdat08 <- data.frame("Company" = str_subset(pdat8,market))
pdat09 <- data.frame("Company" = str_subset(pdat9,market))
pdat010 <- data.frame("Company" = str_subset(pdat10,market))
pdat011 <- data.frame("Company" = str_subset(pdat11,market))
pdat012 <- data.frame("Company" = str_subset(pdat12,market))

#account for potential capitalization
pdat13 <- data.frame("Company" = str_subset(pdat1,cap))
pdat14 <- data.frame("Company" = str_subset(pdat2,cap))
pdat15 <- data.frame("Company" = str_subset(pdat3,cap))
pdat16 <- data.frame("Company" = str_subset(pdat4,cap))
pdat17 <- data.frame("Company" = str_subset(pdat5,cap))
pdat18 <- data.frame("Company" = str_subset(pdat6,cap))
pdat19 <- data.frame("Company" = str_subset(pdat7,cap))
pdat20 <- data.frame("Company" = str_subset(pdat8,cap))
pdat21 <- data.frame("Company" = str_subset(pdat9,cap))
pdat22 <- data.frame("Company" = str_subset(pdat10,cap))
pdat23 <- data.frame("Company" = str_subset(pdat11,cap))
pdat24 <- data.frame("Company" = str_subset(pdat12,cap))

#bind into one data frame
x <- data.frame("Company" = c(0))
x <- rbind(pdat01,pdat02,pdat03,pdat04,pdat05,pdat06,pdat07,pdat08,pdat09,pdat010,pdat011,pdat012)
x <- rbind(x,pdat13,pdat14,pdat15,pdat16,pdat17,pdat18,pdat19,pdat20,pdat21,pdat22,pdat23,pdat24)

#this data frame should look messy: we clean it up with stringr
x$Company <- as.character(x$Company) #no factors yet

#REGEX INFORMATION
#the regex pattern we use is a LookBehind for the 'By' part of 'Marketed By' and all its variations
#We then obtain all the characters past 'by: ' with '?s'.
#Chop off all the remaining words except the first four: ((?:\\S+\\s+){0,3}\\S+)
#still need to clean up some companies with less than four words
for(i in 1:length(x$Company)){
  x$Company[i] <- str_extract(x$Company[i], '(?<=By: |by: |BY: )(?s)((?:\\S+\\s+){0,3}\\S+)')
}
#handle some of the bad company names
x$Company[20] = "H2-Pharma"
x$Company[31] = "McKeon Products"
x$Company[13] = "Merz Aesthetics"
x$Company[22] = "Natrol"
x$Company[21] = "OlivinoLife"
x$Company[19] = "The Mentholatum Company"
#turn back into factor
x$Company <- as.factor(x$Company)

#graphics
pharmTimes17 <- table(x)
pharmTimes17 <- as.data.frame(pharmTimes17)
colnames(pharmTimes17) <- c("Company", "Freq")
#sort only top companies of 2015
pharmTimes17 <- subset(pharmTimes17, Freq >= 2)
#ggplot2
vis <- ggplot(pharmTimes17,aes(x= reorder(Company, -as.numeric(Freq)), y= Freq)) + geom_bar(stat= "identity") +
  scale_y_continuous(limits = c(0,5)) +
  theme_bw()+
  theme(title = element_text(size=20), axis.text.x=element_text(size=18, face = 'bold'), axis.text.y = element_text(size = 15, face = 'bold'),
        axis.title.x = element_text(size = 17, face = 'bold'),axis.title.y = element_text(size = 17, face = 'bold'))+
  theme(plot.title = element_text(size = 28, face = 'bold'))+
  theme(axis.text.x = element_text(angle = 40, hjust = 1)) + xlab("Company") +
  ylab("Times Feautured in News Article") + ggtitle("Top Companies in Pharmacy Times OTC Product News 2017")
vis
ggsave(vis, filename = "otc_2017.png",width=20,height=11.25,scale=1,path = "./src/eiriki/PharmacyTimes/")

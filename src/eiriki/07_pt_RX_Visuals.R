#Prescription Drug Information Pharmacy times
library(dplyr)
library(stringr)
library(ggplot2)
#string we are trying to match
market <- "Marketed"
cap <-"MARKETED"

#pull in all the 2015data
pdat1 <- readLines("./data/business_innovation/working/PHARMACY_TIMES/Rx/34PT_RX_scrape_JANUARY 11, 2015.txt", encoding = "UTF-8")
pdat2 <- readLines("./data/business_innovation/working/PHARMACY_TIMES/Rx/33PT_RX_scrape_FEBRUARY 11, 2015.txt", encoding = "UTF-8")
pdat3 <- readLines("./data/business_innovation/working/PHARMACY_TIMES/Rx/32PT_RX_scrape_MARCH 16, 2015.txt", encoding = "UTF-8")
pdat4 <- readLines("./data/business_innovation/working/PHARMACY_TIMES/Rx/31PT_RX_scrape_APRIL 10, 2015.txt", encoding = "UTF-8")
pdat5 <- readLines("./data/business_innovation/working/PHARMACY_TIMES/Rx/30PT_RX_scrape_MAY 13, 2015.txt", encoding = "UTF-8")
pdat6 <- readLines("./data/business_innovation/working/PHARMACY_TIMES/Rx/29PT_RX_scrape_JUNE 15, 2015.txt", encoding = "UTF-8")
pdat7 <- readLines("./data/business_innovation/working/PHARMACY_TIMES/Rx/28PT_RX_scrape_JULY 10, 2015.txt", encoding = "UTF-8")
pdat8 <- readLines("./data/business_innovation/working/PHARMACY_TIMES/Rx/27PT_RX_scrape_AUGUST 11, 2015.txt", encoding = "UTF-8")
pdat9 <- readLines("./data/business_innovation/working/PHARMACY_TIMES/Rx/26PT_RX_scrape_SEPTEMBER 09, 2015.txt", encoding = "UTF-8")
pdat10 <- readLines("./data/business_innovation/working/PHARMACY_TIMES/Rx/25PT_RX_scrape_OCTOBER 14, 2015.txt", encoding = "UTF-8")
pdat11 <- readLines("./data/business_innovation/working/PHARMACY_TIMES/Rx/24PT_RX_scrape_NOVEMBER 12, 2015.txt", encoding = "UTF-8")
pdat12 <- readLines("./data/business_innovation/working/PHARMACY_TIMES/Rx/23PT_RX_scrape_DECEMBER 16, 2015.txt", encoding = "UTF-8")

#2016 data
pdat13 <- readLines("./data/business_innovation/working/PHARMACY_TIMES/Rx/22PT_RX_scrape_JANUARY 18, 2016.txt", encoding = "UTF-8")
pdat14 <- readLines("./data/business_innovation/working/PHARMACY_TIMES/Rx/21PT_RX_scrape_FEBRUARY 12, 2016.txt", encoding = "UTF-8")
pdat15 <- readLines("./data/business_innovation/working/PHARMACY_TIMES/Rx/20PT_RX_scrape_MARCH 15, 2016.txt", encoding = "UTF-8")
pdat16 <- readLines("./data/business_innovation/working/PHARMACY_TIMES/Rx/19PT_RX_scrape_APRIL 08, 2016.txt", encoding = "UTF-8")
pdat17 <- readLines("./data/business_innovation/working/PHARMACY_TIMES/Rx/18PT_RX_scrape_MAY 13, 2016.txt", encoding = "UTF-8")
pdat18 <- readLines("./data/business_innovation/working/PHARMACY_TIMES/Rx/17PT_RX_scrape_JUNE 10, 2016.txt", encoding = "UTF-8")
pdat19 <- readLines("./data/business_innovation/working/PHARMACY_TIMES/Rx/16PT_RX_scrape_JULY 14, 2016.txt", encoding = "UTF-8")
pdat20 <- readLines("./data/business_innovation/working/PHARMACY_TIMES/Rx/15PT_RX_scrape_AUGUST 18, 2016.txt", encoding = "UTF-8")
pdat21 <- readLines("./data/business_innovation/working/PHARMACY_TIMES/Rx/14PT_RX_scrape_SEPTEMBER 16, 2016.txt", encoding = "UTF-8")
pdat22 <- readLines("./data/business_innovation/working/PHARMACY_TIMES/Rx/13PT_RX_scrape_OCTOBER 17, 2016.txt", encoding = "UTF-8")
pdat23 <- readLines("./data/business_innovation/working/PHARMACY_TIMES/Rx/12PT_RX_scrape_NOVEMBER 20, 2016.txt", encoding = "UTF-8")
pdat24 <- readLines("./data/business_innovation/working/PHARMACY_TIMES/Rx/11PT_RX_scrape_DECEMBER 08, 2016.txt", encoding = "UTF-8")


#store all the text containing "Marketed" in separate lists
pdat1 <- data.frame("Company" = str_subset(pdat1,market))
pdat2 <- data.frame("Company" = str_subset(pdat2,market))
pdat3 <- data.frame("Company" = str_subset(pdat3,market))
pdat4 <- data.frame("Company" = str_subset(pdat4,market))
pdat5 <- data.frame("Company" = str_subset(pdat5,market))
pdat6 <- data.frame("Company" = str_subset(pdat6,market))
pdat7 <- data.frame("Company" = str_subset(pdat7,market))
pdat8 <- data.frame("Company" = str_subset(pdat8,market))
pdat9 <- data.frame("Company" = str_subset(pdat9,market))
pdat10 <- data.frame("Company" = str_subset(pdat10,market))
pdat11 <- data.frame("Company" = str_subset(pdat11,market))
pdat12 <- data.frame("Company" = str_subset(pdat12,market))

#Marketed match again, but for 2016
pdat130 <- data.frame("Company" = str_subset(pdat13,market))
pdat140 <- data.frame("Company" = str_subset(pdat14,market))
pdat150 <- data.frame("Company" = str_subset(pdat15,market))
pdat160 <- data.frame("Company" = str_subset(pdat16,market))
pdat170 <- data.frame("Company" = str_subset(pdat17,market))
pdat180 <- data.frame("Company" = str_subset(pdat18,market))
pdat190 <- data.frame("Company" = str_subset(pdat19,market))
pdat200 <- data.frame("Company" = str_subset(pdat20,market))
pdat210 <- data.frame("Company" = str_subset(pdat21,market))
pdat220 <- data.frame("Company" = str_subset(pdat22,market))
pdat230 <- data.frame("Company" = str_subset(pdat23,market))
pdat240 <- data.frame("Company" = str_subset(pdat24,market))
#Account for change of format "MARKETED"
pdat131 <- data.frame("Company" = str_subset(pdat13,cap))
pdat141 <- data.frame("Company" = str_subset(pdat14,cap))
pdat151 <- data.frame("Company" = str_subset(pdat15,cap))
pdat161 <- data.frame("Company" = str_subset(pdat16,cap))
pdat171 <- data.frame("Company" = str_subset(pdat17,cap))
pdat181 <- data.frame("Company" = str_subset(pdat18,cap))
pdat191 <- data.frame("Company" = str_subset(pdat19,cap))
pdat201 <- data.frame("Company" = str_subset(pdat20,cap))
pdat211 <- data.frame("Company" = str_subset(pdat21,cap))
pdat221 <- data.frame("Company" = str_subset(pdat22,cap))
pdat231 <- data.frame("Company" = str_subset(pdat23,cap))
pdat241 <- data.frame("Company" = str_subset(pdat24,cap))

#bind into one data frame
x <- data.frame("Company" = c(0))
x <- rbind(pdat1,pdat2,pdat3,pdat4,pdat5,pdat6,pdat7,pdat8,pdat9,pdat10,pdat11,pdat12)

y <- data.frame("Company" = c(0))
y <- rbind(pdat130,pdat140,pdat150,pdat160,pdat170,pdat180,pdat190,pdat200,pdat210,pdat220,pdat230,pdat240)
y <- rbind(y,pdat131,pdat141,pdat151,pdat161,pdat171,pdat181,pdat191,pdat201,pdat211,pdat221,pdat231,pdat241)
x$Company <- as.character(x$Company) #no factors yet
y$Company <- as.character(y$Company)
#this section below is using stringr to extract company names
for(i in 1:length(x$Company)){
  x$Company[i] <- str_extract(x$Company[i], '(?<=:\\s)\\S+')
}
for(i in 1:length(y$Company)){
  y$Company[i] <- str_extract(y$Company[i], '(?<=:\\s)\\S+')
}
#put back into factor format
x$Company <- as.factor(x$Company)
y$Company <- as.factor(y$Company)
#quick fix for company names
x$Company <- plyr::revalue(x$Company, c('Actavis,' = "Actavis", 'ActavisIndication:' = "Actavis", Bayer = "Bayer Consumer Healthcare", Galderma = "Galderma Laboratories",
                                        'Amgen,' = "Amgen", 'Eisai,' = "Essai", 'NovartisIndication:' = "Novartis"))

y$Company <- plyr::revalue(y$Company, c('Actavis,' = "Actavis", 'ActavisIndication:' = "Actavis", Bayer = "Bayer Consumer Healthcare", Galderma = "Galderma Laboratories",
                                        'Amgen,' = "Amgen", 'Eisai,' = "Essai", 'NovartisIndication:' = "Novartis", 'AllerganIndication:' = "Allergan",
                                        'AllerganINDICATION:' = "Allergan", 'GlaxoSmithKlineIndication:' = "GlaxoSmithKline",
                                        'JanssenINDICATION:' = "Janssen", 'PfizerIndication:' = "Pfizer", Eli = "Eli Lilly"))

pharmTimes15 <- table(x)
pharmTimes15 <- as.data.frame(pharmTimes15)
pharmTimes16 <- table(y)
pharmTimes16 <- as.data.frame(pharmTimes16)
colnames(pharmTimes15) <- c("Company", "Freq")
colnames(pharmTimes16) <- c("Company", "Freq")
#sort only top companies of 2015
pharmTimes15 <- subset(pharmTimes15, Freq >= 2)
pharmTimes16 <- subset(pharmTimes16, Freq >= 2)
#ggplot2
vis <- ggplot(pharmTimes15,aes(x= reorder(Company, -as.numeric(Freq)), y= Freq)) + geom_bar(stat= "identity") +
  scale_y_continuous(breaks = round(seq(0, max(pharmTimes15$Freq), by = 1),1)) +
  theme_bw()+
  theme(title = element_text(size=20), axis.text.x=element_text(size=10, face = 'bold'), axis.text.y = element_text(size = 10, face = 'bold'),
        axis.title.x = element_text(size = 17, face = 'bold'),axis.title.y = element_text(size = 17, face = 'bold'))+
  theme(plot.title = element_text(size = 20, face = 'bold'))+
  theme(axis.text.x = element_text(angle = 40, hjust = 1)) + xlab("Company") +
  ylab("Times Feautured in News Article") + ggtitle("Top Companies in Pharmacy Times Rx Product News 2015")
vis
#ggsave(vis, filename = "otc_2015.png",width=20,height=11.25,scale=1,path = "./src/eiriki/")

#2016 visualization
vis2 <- ggplot(pharmTimes16,aes(x= reorder(Company, -as.numeric(Freq)), y= Freq)) + geom_bar(stat= "identity") +
  scale_y_continuous(breaks = round(seq(0, max(pharmTimes16$Freq), by = 1),1)) +
  theme_bw()+
  theme(title = element_text(size=20), axis.text.x=element_text(size=10, face = 'bold'), axis.text.y = element_text(size = 10, face = 'bold'),
        axis.title.x = element_text(size = 17, face = 'bold'),axis.title.y = element_text(size = 17, face = 'bold'))+
  theme(plot.title = element_text(size = 20, face = 'bold'))+
  theme(axis.text.x = element_text(angle = 40, hjust = 1)) + xlab("Company") +
  ylab("Times Feautured in News Article") + ggtitle("Top Companies in Pharmacy Times Rx Product News 2016")
vis2

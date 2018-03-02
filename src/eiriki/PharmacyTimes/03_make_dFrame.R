#Code to format all the Pharmacy Times text files into one single data frame

library(dplyr)
library(stringr)
library(ggplot2)
#string we are trying to match
market <- "Marketed"
cap <-"MARKETED"
#pull in all the data
pdat1 <- readLines("./data/business_innovation/working/PHARMACY_TIMES/OTC/35PTscrape_JANUARY 09, 2015.txt", encoding = "UTF-8")
pdat2 <- readLines("./data/business_innovation/working/PHARMACY_TIMES/OTC/34PTscrape_FEBRUARY 08, 2015.txt", encoding = "UTF-8")
pdat3 <- readLines("./data/business_innovation/working/PHARMACY_TIMES/OTC/33PTscrape_MARCH 10, 2015.txt", encoding = "UTF-8")
pdat4 <- readLines("./data/business_innovation/working/PHARMACY_TIMES/OTC/32PTscrape_APRIL 08, 2015.txt", encoding = "UTF-8")
pdat5 <- readLines("./data/business_innovation/working/PHARMACY_TIMES/OTC/31PTscrape_MAY 10, 2015.txt", encoding = "UTF-8")
pdat6 <- readLines("./data/business_innovation/working/PHARMACY_TIMES/OTC/30PTscrape_JUNE 11, 2015.txt", encoding = "UTF-8")
pdat7 <- readLines("./data/business_innovation/working/PHARMACY_TIMES/OTC/29PTscrape_JULY 08, 2015.txt", encoding = "UTF-8")
pdat8 <- readLines("./data/business_innovation/working/PHARMACY_TIMES/OTC/28PTscrape_AUGUST 10, 2015.txt", encoding = "UTF-8")
pdat9 <- readLines("./data/business_innovation/working/PHARMACY_TIMES/OTC/27PTscrape_SEPTEMBER 08, 2015.txt", encoding = "UTF-8")
pdat10 <- readLines("./data/business_innovation/working/PHARMACY_TIMES/OTC/26PTscrape_OCTOBER 13, 2015.txt", encoding = "UTF-8")
pdat11 <- readLines("./data/business_innovation/working/PHARMACY_TIMES/OTC/25PTscrape_NOVEMBER 10, 2015.txt", encoding = "UTF-8")
pdat12 <- readLines("./data/business_innovation/working/PHARMACY_TIMES/OTC/24PTscrape_DECEMBER 15, 2015.txt", encoding = "UTF-8")

#2016 data
pdat13 <- readLines("./data/business_innovation/working/PHARMACY_TIMES/OTC/23PTscrape_JANUARY 15, 2016.txt", encoding = "UTF-8")
pdat14 <- readLines("./data/business_innovation/working/PHARMACY_TIMES/OTC/22PTscrape_FEBRUARY 10, 2016.txt", encoding = "UTF-8")
pdat15 <- readLines("./data/business_innovation/working/PHARMACY_TIMES/OTC/21PTscrape_MARCH 14, 2016.txt", encoding = "UTF-8")
pdat16 <- readLines("./data/business_innovation/working/PHARMACY_TIMES/OTC/20PTscrape_APRIL 07, 2016.txt", encoding = "UTF-8")
pdat17 <- readLines("./data/business_innovation/working/PHARMACY_TIMES/OTC/19PTscrape_MAY 12, 2016.txt", encoding = "UTF-8")
pdat18 <- readLines("./data/business_innovation/working/PHARMACY_TIMES/OTC/18PTscrape_JUNE 08, 2016.txt", encoding = "UTF-8")
pdat19 <- readLines("./data/business_innovation/working/PHARMACY_TIMES/OTC/17PTscrape_JULY 12, 2016.txt", encoding = "UTF-8")
pdat20 <- readLines("./data/business_innovation/working/PHARMACY_TIMES/OTC/16PTscrape_AUGUST 17, 2016.txt", encoding = "UTF-8")
pdat21 <- readLines("./data/business_innovation/working/PHARMACY_TIMES/OTC/15PTscrape_SEPTEMBER 11, 2016.txt", encoding = "UTF-8")
pdat22 <- readLines("./data/business_innovation/working/PHARMACY_TIMES/OTC/14PTscrape_OCTOBER 13, 2016.txt", encoding = "UTF-8")
pdat23 <- readLines("./data/business_innovation/working/PHARMACY_TIMES/OTC/13PTscrape_NOVEMBER 21, 2016.txt", encoding = "UTF-8")
pdat24 <- readLines("./data/business_innovation/working/PHARMACY_TIMES/OTC/12PTscrape_DECEMBER 05, 2016.txt", encoding = "UTF-8")

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
#handle case for tufmed
x$Company[47] <- "TUFMED"
#put back into factor format
x$Company <- as.factor(x$Company)
y$Company <- as.factor(y$Company)
#quick fix for company names
x$Company <- plyr::revalue(x$Company, c(Arm = "Arm & Hammer", Bayer = "Bayer Consumer Healthcare", Galderma = "Galderma Laboratories", Matrixx
                                        = "Matrixx Initiatives", Prestige = "Prestige Brands Holdings", Oxy = "Oxy Bump", Healthy = "Healthy Mama",
                           Procter = "Procter & Gamble", Church = "Church & Dwight", Hello = "Hello Products", Nordic = "Nordic Naturals"))
x$Company <- plyr::revalue(x$Company, c('Chattem,' = "Chattem")) #do this separate because the comma messes it up in previous line
y$Company <- plyr::revalue(y$Company, c(Arm = "Arm & Hammer", Bayer = "Bayer Consumer Healthcare", Galderma = "Galderma Laboratories", Matrixx
                                        = "Matrixx Initiatives", Prestige = "Prestige Brands Holdings", Oxy = "Oxy Bump", Healthy = "Healthy Mama",
                                        Procter = "Procter & Gamble", Church = "Church & Dwight", Hello = "Hello Products", Nordic = "Nordic Naturals"))
y$Company <- plyr::revalue(y$Company, c('Chattem,' = "Chattem")) #do this separate because the comma messes it up in previous line
pharmTimes15 <- table(x)
pharmTimes15 <- as.data.frame(pharmTimes15)
pharmTimes16 <- table(y)
pharmTimes16 <- as.data.frame(pharmTimes16)
colnames(pharmTimes15) <- c("Company", "Freq")
colnames(pharmTimes16) <- c("Company", "Freq")
#sort only top companies of 2015
pharmTimes15 <- subset(pharmTimes15, Freq >= 3)
pharmTimes16 <- subset(pharmTimes16, Freq >= 2)
#ggplot2
vis <- ggplot(pharmTimes15,aes(x= reorder(Company, -as.numeric(Freq)), y= Freq)) + geom_bar(stat= "identity") +
  scale_y_continuous(breaks = round(seq(0, max(pharmTimes15$Freq), by = 1),1)) +
  theme_bw()+
  theme(title = element_text(size=20), axis.text.x=element_text(size=10, face = 'bold'), axis.text.y = element_text(size = 10, face = 'bold'),
        axis.title.x = element_text(size = 17, face = 'bold'),axis.title.y = element_text(size = 17, face = 'bold'))+
  theme(plot.title = element_text(size = 20, face = 'bold'))+
  theme(axis.text.x = element_text(angle = 40, hjust = 1)) + xlab("Company") +
  ylab("Times Feautured in News Article") + ggtitle("Top Companies in Pharmacy Times OTC Product News 2015")
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
  ylab("Times Feautured in News Article") + ggtitle("Top Companies in Pharmacy Times OTC Product News 2016")
vis2
#ggsave(vis2, filename = "otc_2016.png",width=20,height=11.25,scale=1,path = "./src/eiriki/")
# code for later
# market <- "Marketed"
# link <- "For More Information: "
# x <- str_subset(pdat,market)
# x <- str_extract(x, '(?<=:\\s)\\S+')
# x <- str_extract_all(x, boundary("word"))

#this commented out code is for separating each word: will be useful for descriptions later

# dat <- readLines("./data/business_innovation/working/PHARMACY_TIMES/OTC/28PTscrape_AUGUST 10, 2015.txt", encoding = "UTF-8")
# words <- strsplit(dat," ")
# words <- as.data.frame(words)
# wordFrame <- cbind(words[])
# table(words)

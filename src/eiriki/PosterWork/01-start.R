# Exploratory work
library(ggplot2)
library(dplyr)
library(stringr)
source('./src/eiriki/PosterWork/cleaning.R')
#only applying cleaning to Drugs journals
pharmtimes <- read.csv('./data/business_innovation/working/PHARMACY_TIMES/combined/combined.csv')
ptoday <- read.csv('./data/business_innovation/working/PHARMACY_TODAY/pToday.csv')

co_name <- pharmtimes$Company
co_name <- tolower(co_name)
class(co_name)

#run the cleaning function on our data, adjust as needed
x <- cleaning(co_name)
pharmtimes$Company <- x
pharmtimes$Company <- as.factor(pharmtimes$Company)
plot1 <- table(pharmtimes$Company)
plot1 <- as.data.frame(plot1)
colnames(plot1) <- c("Company", "Freq")
plot2 <- subset(plot1, plot1$Freq > 10)
ggplot(plot2, aes(x=Company, y = Freq)) + geom_col()





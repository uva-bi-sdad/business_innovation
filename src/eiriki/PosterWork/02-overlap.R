#overlap between journals and data sources
library(ggplot2)
library(dplyr)
library(stringr)
library(readxl)


tmp1 <- read.csv('./data/business_innovation/working/PHARMACY_TIMES/combined/combined.csv')
tmp2 <- read.csv('./data/business_innovation/working/PHARMACY_TODAY/pToday.csv')
tmp3 <- read_xlsx('./data/business_innovation/original/fda_drugs/Copy of FDA Database - COMBINED V2.xlsx')




pharmtimes <- read.csv('./data/business_innovation/final/Trade_Journals/Pharmacy_Times.csv',stringsAsFactors = F)
ptoday <- read.csv('./data/business_innovation/final/Trade_Journals/Pharmacy_Today.csv',stringsAsFactors = F)
FDA <- read.csv('./data/business_innovation/final/FDA_DRUGS/Fda_Drugs.csv',stringsAsFactors = F)

#check how many products are in FDA and pharmacy times
FDA$Drug.Name <- str_replace(FDA$Drug.Name, " \\#.*$","")
semi_join(pharmtimes,FDA,by='Drug.Name')
#no products match... let's check company matches
x <- semi_join(pharmtimes,FDA,by='Company')
#unique
length(unique(x$Company))


semi_join(ptoday,FDA,by='Drug.Name')
#no products match... let's check company matches
x <- semi_join(ptoday,FDA,by='Company')
#unique
length(unique(x$Company))

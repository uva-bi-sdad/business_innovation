#information for product / company overlaps
#overlap between journals and data sources
library(ggplot2)
library(dplyr)
library(stringr)
library(readxl)
source('./src/eiriki/PosterWork/cleaning.R')

pharmtimes <- read.csv('./data/business_innovation/final/Trade_Journals/Pharmacy_Times.csv',stringsAsFactors = F)
ptoday <- read.csv('./data/business_innovation/final/Trade_Journals/Pharmacy_Today.csv',stringsAsFactors = F)
# FDA <- read.csv('./data/business_innovation/final/FDA_DRUGS/Fda_Drugs.csv',stringsAsFactors = F)
FDA <- read_xlsx('./data/business_innovation/original/fda_drugs/Copy of FDA Database - COMBINED V2.xlsx')
#temp col
FDA$Year <- str_extract(FDA$`Approval Date`,".*?(?=\\-)")
#the goal of this script is to check if Daniel Wilkins numbers are correct for # of products and companies that
# are in originial submission types
# FDA <- FDA
FDA$Submission <- str_replace_all(FDA$Submission,"ORIG.*","ORIG")
# table(FDA$Submission)
#subset by original submissions
orig <- FDA[FDA$Submission == "ORIG",]
others <- FDA[FDA$Submission != "ORIG",]
#check real quick
table(orig$`Submission Classification *`, useNA = "ifany")
sum(table(orig$`Submission Classification *`, useNA = "ifany"))

#take out tentative approvals- we only want full approvals of original submissions
orig <- orig[orig$`Submission Status` != "Tentative Approval",]
others <- others[others$`Submission Status` != "Tentative Approval",]

#groupby
orig_years <- group_by(orig, Year)
orig_years %>% summarise(count = sum(table(Year)))

others_years <- group_by(others, Year)
others_years %>% summarise(count = sum(table(Year)))

#Daniel confirmed to us that the data he used to make the old visuals were using an outdated dataset
#the correct data should be combined V2.So these numbers right here are the correct ones



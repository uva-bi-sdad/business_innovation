#Standardize FDA drugs data
#right now only does suffixes well. things like inc, corp, co. and so on.
library(stringr)

#read in the rules we made in Company_Name_Standardize_2.R
Before_after <- read.csv("./data/business_innovation/working/Company_Standardization_Rules.csv", stringsAsFactors = FALSE)
#read in xlsx file
drugs <- readxl::read_xlsx("./data/business_innovation/original/fda_drugs/Copy of FDA Database - COMBINED V2.xlsx")
ptoday <- read.csv('./data/business_innovation/working/PHARMACY_TODAY/pToday.csv')

#pharmacy times is separated into Rx and OTC so we rbind them together
ptimes <- read.csv('./data/business_innovation/working/PHARMACY_TIMES/combined/otc_dirty.csv')
tmp <- read.csv('./data/business_innovation/working/PHARMACY_TIMES/combined/Rx_dirty.csv')
ptimes <- rbind(ptimes, tmp)
ptimes <- dplyr::filter(ptimes, Year <= 2015, Year >= 2013)

#begin to standardize for each set
Clean_Company_Names <- character(nrow(drugs))
activeName1 = drugs$Company
activeName2 = ptoday$Company
activeName3 = ptimes$Company

#loop through and apply naming rules
for(i in 1:nrow(Before_after)){
  activeName1 = str_replace(activeName1, Before_after[i,1], Before_after[i,2])
  activeName2 = str_replace(activeName2, Before_after[i,1], Before_after[i,2])
  activeName3 = str_replace(activeName3, Before_after[i,1], Before_after[i,2])
}
#check it worked
activeName1

#Three new sets
newD1 <- cbind(drugs, activeName1)
newD2 <- cbind(ptoday, activeName2)
newD3 <- cbind(ptimes, activeName3)

#FIND HOW MANY COMPANIES WE ACTUALLY FIXED
ncompanies_fixed1 <- length(setdiff(newD1$Company, newD1$activeName))
ncompanies_fixed2 <- length(setdiff(newD2$Company, newD2$activeName))
ncompanies_fixed3 <- length(setdiff(newD3$Company, newD3$activeName))

#script for standardization
library(readxl)
library(dplyr)
library(reshape2)
library(stringr)
FDA <- read_xlsx("./data/business_innovation/original/fda_drugs/Copy of FDA Database - COMBINED V2.xlsx")
TJ <- read.csv('./data/business_innovation/working/PHARMACY_TIMES/combined/otc_dirty.csv',stringsAsFactors = F)

#FDA <- read.csv('./data/business_innovation/working/med_device_data/masterkey.csv')
#TJ <- read.csv('./data/business_innovation/working/Medical_Devices_Briefs/Standardized_Company_Names.csv')
#uniques
FDA_vec <- unique(FDA$Company)
TJ_vec <- unique(TJ$Company)

#cleaning
FDA_vec <- tolower(FDA_vec)
TJ_vec <- tolower(TJ_vec)
#still cleaning
FDA_vec <- str_trim(FDA_vec, side = 'both') %>%
  str_replace_all("[[:punct:]]", "") %>%
  str_replace_all(" inc$","") %>%
  str_replace_all(" pharm.*$","") %>%
  str_replace_all(" technologies$","") %>%
  str_replace_all(" intl$", "") %>%
  str_replace_all(" llc$", "") %>%
  str_replace_all(" co$", "")
#still cleaning
TJ_vec <-  str_trim(TJ_vec, side = 'both') %>%
  str_replace_all("[[:punct:]]", "") %>%
  str_replace_all(" inc$","") %>%
  str_replace_all(" pharm.*$","") %>%
  str_replace_all(" technologies$","") %>%
  str_replace_all(" intl$", "") %>%
  str_replace_all(" llc$", "") %>%
  str_replace_all(" co$", "")


FDA_vec <- unique(FDA_vec)
TJ_vec <- unique(TJ_vec)


x <- adist(FDA_vec,TJ_vec)

rownames(x) = FDA_vec
colnames(x) <- TJ_vec

x[1:5,1:5]
table(x)

y <- melt(x)
y$Var1 = as.character(y$Var1)
y$Var2 = as.character(y$Var2)

y$std_dist <- 2*y$value / (nchar(y[,1]) + nchar(y[,2]))

#graphics
hist(y$std_dist)

quantile(y$std_dist,seq(0,1,length =11))

y = y[order(y$std_dist),]
View(head(y, 30))
#saving out to file
#write.csv(y, file= "./data/business_innovation/working/Name Standardization/FDADrugs_Pharmtimes_Companies.csv", row.names = F)



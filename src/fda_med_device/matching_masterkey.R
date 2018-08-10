library(dplyr)
library(reshape2)
library(stringr)
library(data.table)
FDA_MD <- read.csv("./data/business_innovation/working/med_device_data/masterkey.csv", stringsAsFactors = FALSE)
TJ <- read.csv('./data/business_innovation/working/Medical_Devices_Briefs/Standardized_Company_Names.csv',stringsAsFactors = F)


#unique
FDA_vec <- unique(FDA_MD$applicant)
TJ_vec <- unique(TJ$Company_Name)

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
  str_replace_all(" co$", "") %>%
  str_replace_all(" north america$", "") %>%
  str_replace_all(" international$", "") %>%
  str_replace_all(" labs$", "") %>%
  str_replace_all(" products$", "") %>%
  str_replace_all(" corporation.*$", "") %>%
  str_replace_all(" incorporated$", "") %>%
  str_replace_all(" electronics$", "") %>%
  str_replace_all(" medical$", "") %>%
  str_replace_all(" industries$", "") %>%
  str_replace_all(" corp$", "")





#still cleaning
TJ_vec <-  str_trim(TJ_vec, side = 'both') %>%
  str_replace_all("[[:punct:]]", "") %>%
  str_replace_all(" inc$","") %>%
  str_replace_all(" pharm.*$","") %>%
  str_replace_all(" technologies$","") %>%
  str_replace_all(" intl$", "") %>%
  str_replace_all(" llc$", "") %>%
  str_replace_all(" co$", "") %>%
  str_replace_all(" north america$", "") %>%
  str_replace_all(" international$", "") %>%
  str_replace_all(" labs$", "") %>%
  str_replace_all(" products$", "") %>%
  str_replace_all(" corporation.*$", "") %>%
  str_replace_all(" incorporated*$", "") %>%
  str_replace_all(" electronics$", "") %>%
  str_replace_all(" medical$", "") %>%
  str_replace_all(" industries$", "") %>%
  str_replace_all(" corp$", "")






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
#View(head(y, 100))


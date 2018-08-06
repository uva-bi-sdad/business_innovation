# product overlap
library(stringr)
library(dplyr)
library(data.table)
pharmtimes <- read.csv('./data/business_innovation/final/Trade_Journals/Pharmacy_Times.csv',stringsAsFactors = F)
ptoday <- read.csv('./data/business_innovation/final/Trade_Journals/Pharmacy_Today.csv',stringsAsFactors = F)
FDA <- read.csv('./data/business_innovation/final/FDA_DRUGS/Fda_Drugs.csv',stringsAsFactors = F)
#uniques
FDA_vec <- unique(FDA$Company)
TJ_vec <- unique(pharmtimes$Company)
#str dist tells us where the matches are - equal if zero
dist_mat <- adist(FDA_vec,TJ_vec)

rownames(dist_mat) = FDA_vec
colnames(dist_mat) <- TJ_vec

table(dist_mat)
#get zeros
output <- melt(dist_mat)
output$Var1 = as.character(output$Var1)
output$Var2 = as.character(output$Var2)
colnames(output)[1:2] = c("FDA Database", "Pharmacy Times")
output = output[output$value ==0,]
FDA_pharmtimes <- output$`FDA Database`

######run strdist again, but on ptoday------
FDA_vec <- unique(FDA$Company)
TJ_vec <- unique(ptoday$Parent.Company)
#str dist
dist_mat <- adist(FDA_vec,TJ_vec)

rownames(dist_mat) = FDA_vec
colnames(dist_mat) <- TJ_vec

table(dist_mat)
#get zeros
output <- melt(dist_mat)
output$Var1 = as.character(output$Var1)
output$Var2 = as.character(output$Var2)
colnames(output)[1:2] = c("FDA Database", "Pharmacy Today")
output = output[output$value ==0,]
FDA_ptoday <- output$`FDA Database`

#only original submissions is what we want
# FDA$Submission <- str_replace_all(FDA$Submission,"ORIG.*","ORIG")
# # table(FDA$Submission)
# #subset by original submissions
# FDA <- FDA[FDA$Submission == "ORIG",]


##### now that we have both vectors, let's make the FDA vector.
totals1 <- FDA[FDA$Company %in% FDA_pharmtimes,]
totals2 <- FDA[FDA$Company %in% FDA_ptoday,]
FDA_totals <- rbind(totals1,setdiff(totals2,totals1))

tab <- data.frame(table(FDA_totals$Company))
#we have FDA, now get on a trade journal level
totals3 <- pharmtimes[pharmtimes$Company %in% FDA_pharmtimes,]
totals4 <- ptoday[ptoday$Parent.Company %in% FDA_ptoday,]
#drop and rename columns so we can merge together
totals4 <- totals4[ , c(-2,-4)]
colnames(totals4)[2] <- "Company"
#make tables
pharmtimes_totals <- data.frame(table(totals3$Company))
ptoday_totals <- data.frame(table(totals4$Company))
#colnames
colnames(tab) <- c("Company","FDA Frequency")
colnames(pharmtimes_totals) <- c("Company","Pharmacy Times Frequency")
colnames(ptoday_totals) <- c("Company","Pharmacy Today Frequency")
#try to bind together - ignore the warnings
master_table <- full_join(tab,pharmtimes_totals, by = "Company")
master_table <- full_join(master_table,ptoday_totals, by = "Company")
master_table[is.na(master_table)] <- 0

#write out the master table
#write.csv(master_table, file= './data/business_innovation/final/overlap_drugs.csv',row.names = F)


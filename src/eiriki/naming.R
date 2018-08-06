#script for standardization
library(readxl)
library(dplyr)
library(reshape2)
library(stringr)
library(gsubfn)
FDA <- read_xlsx("./data/business_innovation/original/fda_drugs/Copy of FDA Database - COMBINED V2.xlsx")
TJ <- read.csv('./data/business_innovation/working/PHARMACY_TIMES/combined/otc_dirty.csv',stringsAsFactors = F)
#combine the OTC with RX
TJ <- rbind(TJ, read.csv('./data/business_innovation/working/PHARMACY_TIMES/combined/Rx_dirty.csv',stringsAsFactors = F))

#NOTICE THAT THIS IS MATCHING WITH ALL YEARS IN PHARMACY TIMES. MUST SUBSET BY 2013-2015 LATER

# FDA <- read.csv('./data/business_innovation/working/med_device_data/masterkey.csv')
#TJ <- read.csv('./data/business_innovation/working/SurgicalProductCompanyNames.csv')

#variables for before and after
beforeFDA  <- unique(FDA$Company)
beforeTJ <- unique(TJ$Company)

#uniques- MAKE SURE TO CHANGE COMPANY NAME DEPENDING ON THE DATA SOURCE
FDA_vec <- unique(FDA$Company)
TJ_vec <- unique(TJ$Company)
#
# #cleaning
FDA_vec <- tolower(FDA_vec)
TJ_vec <- tolower(TJ_vec)
#still cleaning
FDA_vec <- str_trim(FDA_vec, side = 'both') %>%
  str_replace_all(" \\+ ", " and ") %>%
  str_replace_all("\\,.*", "") %>%
  str_replace_all(": ", "") %>%
  str_replace_all("\\s\\(.*", "") %>%
  str_replace_all("\\;.*", "") %>%
  # str_replace_all("[[:punct:]]", "") %>%
  str_replace_all(" inc$","") %>%
  str_replace_all(" inc $","") %>% #result of taking out (
  str_replace_all(" pharm.*$","") %>%
  str_replace_all(" technologies$","") %>%
  str_replace_all(" intl$", "") %>%
  str_replace_all(" llc$", "") %>%
  str_replace_all(" & co$", "") %>%
  str_replace_all(" co$", "") %>%
  str_replace_all(" north america$", "") %>%
  str_replace_all(" international$", "") %>%
  str_replace_all(" labs$", "") %>%
  str_replace_all(" brands.*$", "") %>%
  # str_replace_all(" products$", "") %>%
  str_replace_all("sanofiaventis", "sanofi aventis") %>%
  str_replace_all(" us$", "") %>%
  str_replace_all(" usa$", "") %>%
  str_replace_all(" corp.*$", "") %>%
  str_replace_all(" industries$", "") %>%
  str_replace_all(" electronics$", "") %>%
  str_replace_all(" medical$", "") %>%
  str_replace_all(" cons ", " consumer ") %>%
  str_replace_all(" cons$", " consumer")%>%
  str_replace_all(" con ", " consumer ")%>%
  str_replace_all(" con$", " consumer")%>%
  str_replace_all(" consumer.*$", "")%>%
  str_replace_all("hlth", "health") %>%
  str_replace_all("health care", "healthcare") %>%
  str_replace_all("glaxosmithkline consumer [^h]", "glaxosmithkline consumer healthcare") %>%
  str_replace_all("philadelphia pa", "") %>%
  str_replace_all("philadelphia", "") %>%
  str_replace_all("pittsburgh", "") %>%
  str_replace_all("glaxosmithkline consumer [^h]", "glaxosmithkline consumer healthcare") %>%
  str_replace_all("philadelphia", "") %>%
  str_replace_all("philadelphia pa", "") %>%
  str_replace_all(" pa(\\s|$)", "") %>%
  str_replace_all(" england", "") %>%
  str_replace_all(" grp ltd", " grp") %>%
  str_replace_all(" denver", "") %>%
  str_replace_all("(?= health(\\s?)care)(.*)", "") %>%
  str_trim(side = 'both') %>%
  str_replace_all(" consumer healthcare.*$", " consumer healthcare") %>%
  str_replace_all(" ireland$", "") %>%
  str_replace_all("johnsonmerck", "johnson-merck") %>%
  str_replace_all("johnson johnson ", "johnson and johnson ") %>%
  str_replace_all("johnson  johnson ", "johnson and johnson ") %>%
  str_replace_all("j and j", "johnson and johnson") %>%
  str_replace_all("johnson and johnson consumer$", "johnson and johnson consumer healthcare") %>%
  str_replace_all("johnson and johnsonconsumer healthcare", "johnson and johnson consumer healthcare") %>%
  str_replace_all("(?= health(\\s?)care)(.*)", " healthcare") %>%
  str_replace_all(" and$", "")%>%
  str_replace_all(" bristolmyers squibb", "bristol myers squibb") %>%
  str_replace_all("sigmatau", "sigma tau") %>%
  str_replace_all(" ip$", "") %>%
  str_replace_all(" laboratories$", "") %>%
  str_replace_all("^lilly$", "eli lilly") %>%
  str_replace_all("^3m.*$", "3m") %>%
  str_replace_all("^abbvie.*$", "abbvie") %>%
  str_replace_all("^actavis.*$", "actavis")%>%
  str_replace_all("^air liquid.*$", "air liquid")%>%
  str_replace_all("^airgas.*$", "airgas")%>%
  str_replace_all("^alkem.*$", "alkem")%>%
  str_replace_all("^alergan.*$", "allergan")%>%
  str_replace_all("^alvogen.*$", "alvogen")%>%
  str_replace_all("^astrazeneca.*$", "astrazeneca")%>%
  str_replace_all("^bristol myers", "bristol-myers")%>%
  str_replace_all("^dr reddys.*$", "dr reddys")%>%
  str_replace_all("^fresenius.*$", "fresenius")%>%
  str_replace_all("^galderma.*$", "galderma")%>%
  str_replace_all("^hetero .*", "hetero") %>%
  str_replace_all("^hoffman la", "hoffman-la")%>%
  str_replace_all("^horizon.*", "horizon")%>%
  str_replace_all("^ipsen.*$", "ipsen") %>%
  str_replace_all("^janssen.*$", "janssen")%>%
  str_replace_all("^linde.*$", "linde")%>%
  str_replace_all("^mallinkrodt.*$", "mallinkrodt")%>%
  str_replace_all("^mylan.*$", "mylan")%>%
  str_replace_all("l perrigo", "perrigo")%>%
  str_replace_all("^perrigo.*$", "perrigo")%>%
  str_replace_all("^praxair.*", "praxair")%>%
  str_replace_all("^ranbaxy.*$", "ranbaxy")%>%
  str_replace_all("^sagent.*$", "sagent")%>%
  str_replace_all("sanofi aventis", "sanofi-aventis")%>%
  str_replace_all("sanofi us services", "sanofi")%>%
  str_replace_all(" holding$", "")%>%
  str_replace_all("^shire.*", "shire")%>%
  str_replace_all("^teva.*$", "teva")%>%
  str_replace_all("^unichem.*$", "unichem")%>%
  str_replace_all("^valeant.*$", "valeant")%>%
  str_replace_all("^wockhardt.*$", "wockhardt")%>%
  str_replace_all("^zydus.*$", "zydus")




#still cleaning
TJ_vec <-  str_trim(TJ_vec, side = 'both') %>%
  str_replace_all(" \\+ ", " and ") %>%
  str_replace_all("\\,.*", "") %>%
  str_replace_all(": ", "") %>%
  str_replace_all("\\s\\(.*", "") %>%
  str_replace_all("\\;.*", "") %>%
  # str_replace_all("[[:punct:]]", "") %>%
  str_replace_all(" inc$","") %>%
  str_replace_all(" inc $","") %>% #result of taking out (
  str_replace_all(" pharm.*$","") %>%
  str_replace_all(" technologies$","") %>%
  str_replace_all(" intl$", "") %>%
  str_replace_all(" llc$", "") %>%
  str_replace_all(" & co$", "") %>%
  str_replace_all(" co$", "") %>%
  str_replace_all(" north america$", "") %>%
  str_replace_all(" international$", "") %>%
  str_replace_all(" labs$", "") %>%
  str_replace_all(" brands.*$", "") %>%
  # str_replace_all(" products$", "") %>%
  str_replace_all("sanofiaventis", "sanofi aventis") %>%
  str_replace_all(" us$", "") %>%
  str_replace_all(" usa$", "") %>%
  str_replace_all(" corp.*$", "") %>%
  str_replace_all(" industries$", "") %>%
  str_replace_all(" electronics$", "") %>%
  str_replace_all(" medical$", "") %>%
  str_replace_all(" cons ", " consumer ") %>%
  str_replace_all(" cons$", " consumer")%>%
  str_replace_all(" con ", " consumer ")%>%
  str_replace_all(" con$", " consumer")%>%
  str_replace_all(" consumer.*$", "")%>%
  str_replace_all("hlth", "health") %>%
  str_replace_all("health care", "healthcare") %>%
  str_replace_all("glaxosmithkline consumer [^h]", "glaxosmithkline consumer healthcare") %>%
  str_replace_all("philadelphia pa", "") %>%
  str_replace_all("philadelphia", "") %>%
  str_replace_all("pittsburgh", "") %>%
  str_replace_all("glaxosmithkline consumer [^h]", "glaxosmithkline consumer healthcare") %>%
  str_replace_all("philadelphia", "") %>%
  str_replace_all("philadelphia pa", "") %>%
  str_replace_all(" pa(\\s|$)", "") %>%
  str_replace_all(" england", "") %>%
  str_replace_all(" grp ltd", " grp") %>%
  str_replace_all(" denver", "") %>%
  str_replace_all("(?= health(\\s?)care)(.*)", "") %>%
  str_trim(side = 'both') %>%
  str_replace_all(" consumer healthcare.*$", " consumer healthcare") %>%
  str_replace_all(" ireland$", "") %>%
  str_replace_all("johnsonmerck", "johnson-merck") %>%
  str_replace_all("johnson johnson ", "johnson and johnson ") %>%
  str_replace_all("johnson  johnson ", "johnson and johnson ") %>%
  str_replace_all("j and j", "johnson and johnson") %>%
  str_replace_all("johnson and johnson consumer$", "johnson and johnson consumer healthcare") %>%
  str_replace_all("johnson and johnsonconsumer healthcare", "johnson and johnson consumer healthcare") %>%
  str_replace_all("(?= health(\\s?)care)(.*)", " healthcare") %>%
  str_replace_all(" and$", "")%>%
  str_replace_all(" bristolmyers squibb", "bristol myers squibb") %>%
  str_replace_all("sigmatau", "sigma tau") %>%
  str_replace_all(" ip$", "") %>%
  str_replace_all(" laboratories$", "") %>%
  str_replace_all("^lilly$", "eli lilly") %>%
  str_replace_all("^3m.*$", "3m") %>%
  str_replace_all("^abbvie.*$", "abbvie") %>%
  str_replace_all("^actavis.*$", "actavis")%>%
  str_replace_all("^air liquid.*$", "air liquid")%>%
  str_replace_all("^airgas.*$", "airgas")%>%
  str_replace_all("^alkem.*$", "alkem")%>%
  str_replace_all("^alergan.*$", "allergan")%>%
  str_replace_all("^alvogen.*$", "alvogen")%>%
  str_replace_all("^astrazeneca.*$", "astrazeneca")%>%
  str_replace_all("^bristol myers", "bristol-myers")%>%
  str_replace_all("^dr reddys.*$", "dr reddys")%>%
  str_replace_all("^fresenius.*$", "fresenius")%>%
  str_replace_all("^galderma.*$", "galderma")%>%
  str_replace_all("^hetero .*", "hetero") %>%
  str_replace_all("^hoffman la", "hoffman-la")%>%
  str_replace_all("^horizon.*", "horizon")%>%
  str_replace_all("^ipsen.*$", "ipsen") %>%
  str_replace_all("^janssen.*$", "janssen")%>%
  str_replace_all("^linde.*$", "linde")%>%
  str_replace_all("^mallinkrodt.*$", "mallinkrodt")%>%
  str_replace_all("^mylan.*$", "mylan")%>%
  str_replace_all("l perrigo", "perrigo")%>%
  str_replace_all("^perrigo.*$", "perrigo")%>%
  str_replace_all("^praxair.*", "praxair")%>%
  str_replace_all("^ranbaxy.*$", "ranbaxy")%>%
  str_replace_all("^sagent.*$", "sagent")%>%
  str_replace_all("sanofi aventis", "sanofi-aventis")%>%
  str_replace_all("sanofi us services", "sanofi")%>%
  str_replace_all(" holding$", "")%>%
  str_replace_all("^shire.*", "shire")%>%
  str_replace_all("^teva.*$", "teva")%>%
  str_replace_all("^unichem.*$", "unichem")%>%
  str_replace_all("^valeant.*$", "valeant")%>%
  str_replace_all("^wockhardt.*$", "wockhardt")%>%
  str_replace_all("^zydus.*$", "zydus")



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
colnames(y)[1:2] = c("FDA Database", "Pharmacy Times")
#y = dplyr::filter(y, std_dist < 1)
View(head(y, 100))

#saving out to file
#write.csv(y, file= "./data/business_innovation/working/Name Standardization/FDADrugs_Pharmtimes_Companies.csv", row.names = F)




#
# ##### new table
# beforeFDA <- beforeFDA[1:50]
# beforeFDA <- tolower(beforeFDA)
#
# afterFDA <-  str_trim(beforeFDA, side = 'both') %>%
#   str_replace_all(" \\+ ", " and ") %>%
#   str_replace_all("[[:punct:]]", "") %>%
#   str_replace_all(" inc$","") %>%
#   str_replace_all(" pharm.*$","") %>%
#   str_replace_all(" technologies$","") %>%
#   str_replace_all(" intl$", "") %>%
#   str_replace_all(" llc$", "") %>%
#   str_replace_all(" co$", "") %>%
#   str_replace_all(" north america$", "") %>%
#   str_replace_all(" international$", "") %>%
#   str_replace_all(" labs$", "") %>%
#   str_replace_all(" products$", "")%>%
#   str_replace_all("sanofiaventis", "sanofi aventis")%>%
#   str_replace_all(" us$", "") %>%
#   str_replace_all(" corp.*$", "") %>%
#   str_replace_all(" industries$", "") %>%
#   str_replace_all(" electronics$", "") %>%
#   str_replace_all(" medical$", "") %>%
#   str_replace_all(" cons ", " consumer ") %>%
#   str_replace_all(" cons$", " consumer")%>%
#   str_replace_all(" con ", " consumer ")%>%
#   str_replace_all(" con$", " consumer")%>%
#   str_replace_all("hlth", "health") %>%
#   str_replace_all("health care", "healthcare") %>%
#   str_replace_all("glaxosmithkline consumer [^h]", "glaxosmithkline consumer healthcare") %>%
#   str_replace_all("philadelphia pa", "") %>%
#   str_replace_all("philadelphia", "") %>%
#   str_replace_all("pittsburgh", "") %>%
#   str_replace_all(" pa(\\s|$)", "") %>%
#   str_replace_all(" england", "") %>%
#   str_replace_all(" grp ltd", " grp") %>%
#   str_replace_all(" denver", "") %>%
#   str_trim(side = 'both') %>%
#   str_replace_all(" consumer healthcare.*$", " consumer healthcare") %>%
#   str_replace_all(" ireland$", "") %>%
#   str_replace_all("johnsonmerck", "johnson-merck") %>%
#   str_replace_all("johnson johnson ", "johnson and johnson ") %>%
#   str_replace_all("johnson  johnson ", "johnson and johnson ") %>%
#   str_replace_all("j and j", "johnson and johnson") %>%
#   str_replace_all("johnson and johnson consumer$", "johnson and johnson consumer healthcare") %>%
#   str_replace_all("johnson and johnsonconsumer healthcare", "johnson and johnson consumer healthcare") %>%
#   str_replace_all("(?= health(\\s?)care)(.*)", " healthcare") %>%
#   str_replace_all(" and$", "") %>%
#   str_replace_all(" bristolmyers squibb", "bristol myers squibb") %>%
#   str_replace_all("sigmatau", "sigma tau")
# beforeFDA
# afterFDA

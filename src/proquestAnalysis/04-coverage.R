#quick script for data coverage of proquest

#libraries
library(dplyr)

#load data

# dat <- read.csv('./data/business_innovation/working/parsedProquestData/naics325412.csv')
# pharmaFiles = list.files("./data/business_innovation/working/parsedProquestData/", full.names = TRUE)[c(3, 4, 11, 12, 13)]
# keywords<-c("launch","new product","product release")

scrapedFiles = list.files("./data/business_innovation/working/parsedProquestData", full.names = TRUE)
scrapedFiles = scrapedFiles[5:10]
length(scrapedFiles)
scrapedFiles

dat <- read.csv(scrapedFiles[1])
tab <- table(dat$Source.type)

dat2 <- read.csv(scrapedFiles[2])
tab2 <- table(dat2$Source.type)

dat3 <- read.csv(scrapedFiles[3])
tab3 <- table(dat3$Source.type)

dat4 <- read.csv(scrapedFiles[4])
tab4 <- table(dat4$Source.type)

dat5 <- read.csv(scrapedFiles[5])
tab5 <- table(dat5$Source.type)

dat6 <- read.csv(scrapedFiles[6])
tab6 <- table(dat6$Source.type)

#combine and clean
coverage <- bind_rows(tab,tab2,tab3,tab4,tab5,tab6)
coverage['Sector Name'] <- c('1','2','3','4','5','6')
coverage['NAICS Code'] <- c('1','2','3','4','5','6')
coverage['Total Articles'] <- c('1','2','3','4','5','6')
coverage
coverage <- coverage[c(10,11,12,1:9)] #reorder columns
coverage[is.na(coverage)] <- 0 #replace Na with 0

sector_names <- c('Pharmaceutical Preparation Manufacturing', 'Turbine and Turbine Generator Set Units Manufacturing',
                  'Software Publishers', 'Data Processing, Hosting, and Related Services',
                  'Research and Development in Biotechnology', 'Research and Development in the Physical, Engineering, and Life Sciences')
naics_n <- c(325412,336111,511210,518210,541711,541712)
totals_n <- rowSums(coverage[4:12])
coverage['Sector Name'] <- sector_names
coverage['NAICS Code'] <- naics_n
coverage['Total Articles'] <- totals_n
coverage

#use this to write out
#write_csv(coverage, path = './data/business_innovation/working/parsedProquestData/coverage.csv')

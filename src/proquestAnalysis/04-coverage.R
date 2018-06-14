#quick script for data coverage of proquest

#libraries
library(dplyr)
library(stringr)
library(reshape2)
library(ggplot2)
library(viridis)
library(scales)
library(lubridate)
library(data.table)
library(forcats)
library(xtable)

#load data

# dat <- read.csv('./data/business_innovation/working/parsedProquestData/naics325412.csv')
# pharmaFiles = list.files("./data/business_innovation/working/parsedProquestData/", full.names = TRUE)[c(3, 4, 11, 12, 13)]
# keywords<-c("launch","new product","product release")

scrapedFiles = list.files("./data/business_innovation/working/parsedProquestData", full.names = TRUE)
scrapedFiles = scrapedFiles[5:10]
length(scrapedFiles)

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

coverage <- bind_rows(tab,tab2,tab3,tab4,tab5,tab6)

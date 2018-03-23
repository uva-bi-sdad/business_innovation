# Explore tech crunch data
library(dplyr)
library(stringr)
library(reshape2)
library(ggplot2)
library(viridis)
library(scales)
library(lubridate)
library(data.table)

# upload cleaned data
scrapedFiles = list.files("./data/business_innovation/working/ParsedVTLibData/Proquest", full.names = TRUE)
ncompanies = length(scrapedFiles)
keywords<-c("launch","new product","product release")
pal <- rev(viridis_pal(alpha = 1, begin = 0, end = 1, direction = 1, option = "A")(10))[c(4,8)]
#company by naics

companyNaics = fread("./data/business_innovation/working/companyToNaicsProQuest.csv")

# Set scrape names
scrapedNames = c("Ford Motors", "General Motors", "GlaxoSmithKline", "Merck", "NAICS 336111", "NAICS 511210", "NAICS 518210", "NAICS 541711", "NAICS 541712", "Novartis", "Pfizer", "Proctor & Gamble", "Tata Motors", "Toyota", "Volkswagen")

# For auto (then the other codes) find out what company each article is about. Then for each company, count the number of innovation articles

# 1) Each article has a list of companies attached. Count the number of times the company appears in the text body and say that the model company is the subject of the article. Only do this for 'innovation articles'.

activeFile = unique(fread(scrapedFiles[5]), by = 'Article.Title')
activeFile = filter(activeFile, grepl(paste(keywords, collapse = '|'), activeFile$Full.Text))

companyList = str_extract_all(activeFile$Company, "(?<=Name: )(.*?)(?=;)")
table(sapply(companyList, length))/sum(table(sapply(companyList, length)))

# Because company names don't exactly show up as listed in the Company column, we need a function which takes a company name and makes some good regex











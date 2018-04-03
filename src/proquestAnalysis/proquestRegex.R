# Explore tech crunch data
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


# upload cleaned data
scrapedFiles = list.files("./data/business_innovation/working/ParsedVTLibData/Proquest", full.names = TRUE)
ncompanies = length(scrapedFiles)
keywords<-c("launch","new product","product release")

#company by naics

companyNaics = fread("./data/business_innovation/working/companyToNaicsProQuest.csv")

fullCompanyRegex = regexFromCompanyName(unique(companyNaics$Company, by = "Company"))

# Choose one of the files to work with
activeFile = unique(fread(scrapedFiles[5]), by = 'Article.Title')
activeFile = filter(activeFile, grepl(paste(keywords, collapse = '|'), activeFile$Full.Text))
companyList = str_extract_all(activeFile$Company, "(?<=Name: )(.*?)(?=;)")

names = companyList[[i]]

regexFromCompanyName = function(names){
  regexOut = vector("list", length(names))
  for(i in 1:length(names)){
    words = unlist(strsplit(names[i], " "))
    words = str_replace_all(words, "[\\(\\)]", "")
    nwords = length(words)
    indices = sapply(nwords:1, function(x) 1:x)
    patterns = sapply(indices, function(x) paste0(words[x], collapse = " "))
    # Remove strings of length <= 3
    patterns = patterns[nchar(patterns) > 3]
    regexOut[[i]] = patterns
  }
  regexOut = regexOut[sapply(regexOut, length) > 0]
  return(regexOut)
}


i = 4

companyRegex = regexFromCompanyName(companyList[[i]])
companyRegex
activeFile[i, ]
str_extract_all( activeFile[i, 9], companyList[[i]])
str_extract_all( activeFile[i, 9], companyRegex)







tmpMatch = str_extract_all( activeFile[i, 9], fullCompanyRegex)
tmpMatch[sapply(tmpMatch, function(x) length(x) > 0)]

# Find terms to remove from the above fullCompanyRegex
# china, european, bank, anything shorter than 3 words, association, group, Japan, Ann, Society, Economic,

fullCompanyRegex[sapply(tmpMatch, function(x) length(x) > 0)]






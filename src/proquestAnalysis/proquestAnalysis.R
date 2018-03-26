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
# The above proved to be hard, so I'm just doing companies by article for now.

# This makes a table of 1) indicators for each article and 2) companies that show in that article
articleIndicator = rep(1:length(companyList), sapply(companyList, length))
companyByArticle = data.table(article = articleIndicator, company = unlist(companyList))

articlesPerCompany = arrange(companyByArticle[,.N, by = company], -N)
top12CompaniesNAICS = articlesPerCompany[1:12,]
top12CompaniesNAICS$company = fct_infreq(top12CompaniesNAICS$company)
top12CompaniesNAICS$color = as.character(c(1, 1, 1, 2, 2, 2, 2, 2, 2, 1, 2, 1))

# Plot the top 12 companies from the NAICS code search

naicsInnovationPlot = ggplot(data = top12CompaniesNAICS) +
  geom_bar(aes(x = fct_inorder(company), fill = color, y = N), stat ="identity") +
  theme_bw() +
  scale_fill_manual(values = pal,name = "") +
  theme(axis.text.x=element_text(size=14, angle = 45, hjust = 1, vjust = 1),
        axis.text.y=element_text(size=14),
        axis.title=element_text(size=16),
        title=element_text(size=20),
        legend.position="none",
        legend.text = element_text(size=16)) +
  ylim(0, 210) +
  labs(x = "Company",y = "Number of Articles", title = "Top 12 Mentions From NAICS 336111")

# Make the above plot using data from the 5 companies we scraped individually

autoFiles = list.files("./data/business_innovation/working/ParsedVTLibData/Proquest", full.names = TRUE)[c(1, 2, 13, 14, 15)]
keywords<-c("launch","new product","product release")

# Set plotting parameters to loop over
companyNames = c("Ford Motors", "General Motors", "Tata Motors", "Toyota", "Volkswagen")
keywordArticles = numeric(5)
for(i in 1:5){
  activeFile = fread(autoFiles[i], stringsAsFactors = FALSE)
  activeFile = unique(activeFile, by = 'Article.Title')

  keywordArticles[i] = sum(grepl(paste(keywords, collapse = '|'), activeFile$Full.Text))
}

keywordArticlesCompanySearch = arrange(data.table(company = companyNames, N = keywordArticles), by = -N)

individualInnovationPlot = ggplot(data = keywordArticlesCompanySearch) +
  geom_bar(aes(x = fct_inorder(company), y = N, fill = rep("1", 5)), stat ="identity") +
  theme_bw() +
  scale_fill_manual(values = pal,name = "") +
  theme(axis.text.x=element_text(size=14, angle = 45, hjust = 1, vjust = 1),
        axis.text.y=element_text(size=14),
        axis.title=element_text(size=16),
        title=element_text(size=20),
        legend.position="none",
        legend.text = element_text(size=16)) +
  ylim(0, 210) +
  labs(x = "Company",y = "Number of Articles", title = "Mentions from Individually Searched Companies")

pdf("./output/innovationPlots/innovationArticlesByCompanyFromNaics.pdf", width = 12, height = 8)
plot(naicsInnovationPlot)
dev.off()

pdf("./output/innovationPlots/innovationArticlesByCompanyIndividually.pdf", width = 12, height = 8)
plot(individualInnovationPlot)
dev.off()

# Write a function to make a basic summary of each scraped file

scrapedFiles = list.files("./data/business_innovation/working/ParsedVTLibData/Proquest", full.names = TRUE)
ncompanies = length(scrapedFiles)
keywords<-c("launch","new product","product release")

fileName = scrapedFiles[1]

summarizeProquestFile = function(fileName){
  file = fread(fileName)
  out = data.table(fileName = str_extract(fileName,"(?<=Proquest/)(.*)"),
                   nRecords = nrow(file),
                   nInnoRecords = sum(grepl(paste(keywords, collapse = '|'), file$Full.Text)),
                   startDate = range(na.omit(file$Publication.date))[1],
                   endDate = range(na.omit(file$Publication.date))[2]
                   )
  return(out)
}

summaries = data.table()

for(i in 1:ncompanies) summaries = rbind(summaries, summarizeProquestFile(scrapedFiles[i]))

summaries = arrange(summaries, -nRecords)
print(xtable(summaries), include.rownames=FALSE)





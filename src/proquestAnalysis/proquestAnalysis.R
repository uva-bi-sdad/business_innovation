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
pal <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

#company by naics
companyNaics = fread("./data/business_innovation/working/companyToNaicsProQuest.csv")


# For auto (then the other codes) find out what company each article is about. Then for each company, count the number of innovation articles

# 1) Each article has a list of companies attached. Count the number of times the company appears in the text body and say that the model company is the subject of the article. Only do this for 'innovation articles'.

activeFile = unique(fread(scrapedFiles[6]), by = 'Article.Title')
activeFile = activeFile[grepl(paste(keywords, collapse = '|'), activeFile$Full.Text),]

companyList = str_extract_all(activeFile$Company, "(?<=Name: )(.*?)(?=;)")

# Because company names don't exactly show up as listed in the Company column, we need a function which takes a company name and makes some good regex
# The above proved to be hard, so I'm just doing companies by article for now.

# This makes a table of 1) indicators for each article and 2) companies that show in that article
articleIndicator = rep(1:length(companyList), sapply(companyList, length))
year = factor(rep(year(activeFile$Publication.date), sapply(companyList, length)), levels = c("2015", "2014", "2013"))
companyByArticle = data.table(article = articleIndicator, company = unlist(companyList), year = year)

# Find top 13 companies
articlesPerCompany = arrange(companyByArticle[,.N, by = company], -N)
top13CompaniesNAICS = articlesPerCompany[1:13,]
top13CompaniesNAICS$company = fct_infreq(top13CompaniesNAICS$company)


# Break out top 13 by year
articlesPerCompanyYear = arrange(companyByArticle[company %in% top13CompaniesNAICS$company,.N, by = .(company, year)], -N)

# Plot the top 13 companies from the NAICS code search

naicsInnovationPlot =
  ggplot(data = articlesPerCompanyYear) +
  geom_bar(aes(x = fct_inorder(company), fill = year, y = N), stat ="identity") +
  theme_bw() +
  scale_fill_manual(values = pal,name = "") +
  theme(axis.text.x=element_text(size=14, angle = 45, hjust = 1, vjust = 1),
        axis.text.y=element_text(size=14),
        axis.title=element_text(size=16),
        title=element_text(size=20),
        legend.text = element_text(size=16)) +
  ylim(0, 210) +
  labs(x = "Company",y = "Number of Articles", title = "Top 13 Mentions From NAICS 336111")

# Make the above plot using data from the 5 companies we scraped individually

autoFiles = list.files("./data/business_innovation/working/ParsedVTLibData/Proquest", full.names = TRUE)[c(1, 2, 14, 15, 16)]
keywords<-c("launch","new product","product release")

# Set plotting parameters to loop over
companyNames = c("Ford Motors", "General Motors", "Tata Motors", "Toyota", "Volkswagen")
keywordArticlesYear = vector('list', 5)
for(i in 1:5){
  activeFile = fread(autoFiles[i], stringsAsFactors = FALSE)
  activeFile = unique(activeFile, by = 'Article.Title')
  activeFile = activeFile[grepl(paste(keywords, collapse = '|'), activeFile$Full.Text),]


  keywordArticlesYear[[i]] = cbind(company = companyNames[i],na.omit(activeFile[,.N, by = year(Publication.date)]))
}

keywordArticlesCompanySearch = do.call('rbind', keywordArticlesYear)

individualInnovationPlot =
  ggplot(data = keywordArticlesCompanySearch) +
  geom_bar(aes(x = fct_inorder(company), fill = factor(year, levels = c("2015", "2014", "2013")), y = N), stat ="identity") +
  theme_bw() +
  scale_fill_manual(values = pal,name = "") +
  theme(axis.text.x=element_text(size=14, angle = 45, hjust = 1, vjust = 1),
        axis.text.y=element_text(size=14),
        axis.title=element_text(size=16),
        title=element_text(size=20),
        #legend.position="none",
        legend.text = element_text(size=16)) +
  ylim(0, 210) +
  labs(x = "Company",y = "Number of Articles", title = "Mentions from Individually Searched Companies")

pdf("./output/innovationPlots/innovationArticlesByCompanyFromNaicsAuto.pdf", width = 12, height = 8)
plot(naicsInnovationPlot)
dev.off()

pdf("./output/innovationPlots/innovationArticlesByCompanyIndividuallyAuto.pdf", width = 12, height = 8)
plot(individualInnovationPlot)
dev.off()


#
# MAKE THE SAME PLOTS AS ABOVE FOR PHARMA
#

activeFile = unique(fread(scrapedFiles[5]), by = 'Article.Title')
activeFile = activeFile[grepl(paste(keywords, collapse = '|'), activeFile$Full.Text),]

companyList = str_extract_all(activeFile$Company, "(?<=Name: )(.*?)(?=;)")

# Because company names don't exactly show up as listed in the Company column, we need a function which takes a company name and makes some good regex
# The above proved to be hard, so I'm just doing companies by article for now.

# This makes a table of 1) indicators for each article and 2) companies that show in that article
articleIndicator = rep(1:length(companyList), sapply(companyList, length))
year = factor(rep(year(activeFile$Publication.date), sapply(companyList, length)), levels = c("2015", "2014", "2013"))
companyByArticle = data.table(article = articleIndicator, company = unlist(companyList), year = year)

# Find top 13 companies
articlesPerCompany = na.omit(arrange(companyByArticle[,.N, by = company], -N))
top13CompaniesNAICS = articlesPerCompany[1:13,]
top13CompaniesNAICS$company = fct_infreq(top13CompaniesNAICS$company)


# Break out top 13 by year and rename some long ones
articlesPerCompanyYear = na.omit(arrange(companyByArticle[company %in% top13CompaniesNAICS$company,.N, by = .(company, year)], -N))
articlesPerCompanyYear$company[articlesPerCompanyYear$company == "Gilead Sciences Inc"] = "Gilead"
articlesPerCompanyYear$company[articlesPerCompanyYear$company == "Food & Drug Administration--FDA"] = "FDA"
articlesPerCompanyYear$company = factor(articlesPerCompanyYear$company, levels = c("Pfizer Inc", "FDA", "GlaxoSmithKline PLC", "Gilead", "Procter & Gamble Co", "Merck & Co Inc", "Johnson & Johnson", "Novartis AG", "Eli Lilly & Co", "Bristol-Myers Squibb Co", "AbbVie Inc", "AstraZeneca PLC", "Amgen Inc"))
# Plot the top 13 companies from the NAICS code search

naicsInnovationPlot =
  ggplot(data = articlesPerCompanyYear) +
  geom_bar(aes(x = company, fill = year, y = N), stat ="identity") +
  theme_bw() +
  scale_fill_manual(values = pal,name = "") +
  theme(axis.text.x=element_text(size=14, angle = 45, hjust = 1, vjust = 1),
        axis.text.y=element_text(size=14),
        axis.title=element_text(size=16),
        title=element_text(size=20),
        legend.text = element_text(size=16)) +
  ylim(0, 90) +
  labs(x = "Company",y = "Number of Articles", title = "Top 13 Mentions From NAICS 325412")

# Make the above plot using data from the 5 companies we scraped individually

pharmaFiles = list.files("./data/business_innovation/working/ParsedVTLibData/Proquest", full.names = TRUE)[c(3, 4, 11, 12, 13)]
keywords<-c("launch","new product","product release")

# Set plotting parameters to loop over
companyNames = c("GlaxoSmithKline", "Merck", "Novartis", "Pfizer", "P&G")
keywordArticlesYear = vector('list', 5)
for(i in 1:5){
  activeFile = fread(pharmaFiles[i], stringsAsFactors = FALSE)
  activeFile = unique(activeFile, by = 'Article.Title')
  activeFile = activeFile[grepl(paste(keywords, collapse = '|'), activeFile$Full.Text),]


  keywordArticlesYear[[i]] = cbind(company = companyNames[i],na.omit(activeFile[year(Publication.date) %in% c('2013', '2014', '2015'),.N, by = year(Publication.date)]))
}

keywordArticlesCompanySearch = do.call('rbind', keywordArticlesYear)
keywordArticlesCompanySearch$company = factor(keywordArticlesCompanySearch$company, levels = c("Pfizer", "GlaxoSmithKline", "Merck", "P&G", "Novartis"))

individualInnovationPlot =
  ggplot(data = keywordArticlesCompanySearch) +
  geom_bar(aes(x = company, fill = factor(year, levels = c("2015", "2014", "2013")), y = N), stat ="identity") +
  theme_bw() +
  scale_fill_manual(values = pal,name = "") +
  theme(axis.text.x=element_text(size=14, angle = 45, hjust = 1, vjust = 1),
        axis.text.y=element_text(size=14),
        axis.title=element_text(size=16),
        title=element_text(size=20),
        #legend.position="none",
        legend.text = element_text(size=16)) +
  ylim(0, 100) +
  labs(x = "Company",y = "Number of Articles", title = "Mentions from Individually Searched Companies")

pdf("./output/innovationPlots/innovationArticlesByCompanyFromNaicsPharma.pdf", width = 12, height = 8)
plot(naicsInnovationPlot)
dev.off()

pdf("./output/innovationPlots/innovationArticlesByCompanyIndividuallyPharma.pdf", width = 12, height = 8)
plot(individualInnovationPlot)
dev.off()

#
# Write a function to make a basic summary of each scraped file
#

scrapedFiles = list.files("./data/business_innovation/working/ParsedVTLibData/Proquest", full.names = TRUE)
ncompanies = length(scrapedFiles)
keywords<-c("launch","new product","product release")


summarizeProquestFile = function(fileName){
  file = fread(fileName)

  out = data.table(fileName = str_extract(fileName,"(?<=Proquest/)(.*)(?=\\.csv)"),
                   nRecords = nrow(file),
                   nInnoRecords = sum(grepl(paste(keywords, collapse = '|'), file$Full.Text)),
                   nLaunch = sum(grepl(keywords[1], file$Full.Text)),
                   nNewProduct = sum(grepl(keywords[2], file$Full.Text)),
                   nProductRelease = sum(grepl(keywords[3], file$Full.Text)),
                   startDate = range(na.omit(file$Publication.date))[1],
                   endDate = range(na.omit(file$Publication.date))[2]
  )
  return(out)
}

summaries = data.table()

for(i in 1:ncompanies) summaries = rbind(summaries, summarizeProquestFile(scrapedFiles[i]))

summaries = arrange(summaries, -nRecords)
summaries = summaries[c(1, 2, 5, 7, 15, 3, 4, 6, 9, 11, 8, 10, 12, 13, 14),]
print(xtable(summaries[,-c(7,8)]), include.rownames=FALSE)

# Find number of unique companies mentioned in all articles for a NAICS code, and then for new release articles

naicsFiles = scrapedFiles[5:9]

extractUniqueCompanies = function(inFile){

  activeFile = unique(fread(inFile), by = 'Article.Title')
  nCompaniesAll = length(unique(unlist(str_extract_all(activeFile$Company, "(?<=Name: )(.*?)(?=;)"))))
  activeFile = activeFile[grepl(paste(keywords, collapse = '|'), activeFile$Full.Text),]
  nCompaniesInno = length(unique(unlist(str_extract_all(activeFile$Company, "(?<=Name: )(.*?)(?=;)"))))
  return(data.frame(allArticleMentions = nCompaniesAll, innoArticleMentions = nCompaniesInno))
}

sapply(naicsFiles, extractUniqueCompanies)





articleIndicator = rep(1:length(companyList), sapply(companyList, length))
year = factor(rep(year(activeFile$Publication.date), sapply(companyList, length)), levels = c("2015", "2014", "2013"))
companyByArticle = data.table(article = articleIndicator, company = unlist(companyList), year = year)

# Find top 13 companies
articlesPerCompany = arrange(companyByArticle[,.N, by = company], -N)




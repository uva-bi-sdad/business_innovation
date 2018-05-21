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
scrapedFiles = list.files("./data/business_innovation/working/parsedProquestData/", full.names = TRUE)
ncompanies = length(scrapedFiles)
keywords<-c("launch","new product","product release")
pal <- rev(viridis_pal(alpha = 1, begin = 0, end = 1, direction = 1, option = "A")(10))[c(4,8)]

# Set plotting parameters to loop over
mains = c("Ford Motors", "General Motors", "GlaxoSmithKline", "Merck", "NAICS 336111", "NAICS 511210", "NAICS 518210", "NAICS 541711", "NAICS 541712", "Novartis", "Pfizer", "Proctor & Gamble", "Tata Motors", "Toyota", "Volkswagen")

# Plots for number of articles and number of new product articles
for(i in 1:ncompanies){
  activeFile = fread(scrapedFiles[i], stringsAsFactors = FALSE)
  activeFile = unique(activeFile, by = 'Article.Title')

  articleHasKeyword = grepl(paste(keywords, collapse = '|'), activeFile$Full.Text)
  articleYear = year(ymd(activeFile$Publication.date))
  data.frame(articleYear, articleHasKeyword) %>%
    group_by(articleYear, articleHasKeyword) %>%
    summarise(n = n()) -> plotData



  #pdf("new_prod_articles_year.pdf",width=6,height=5)
  p = ggplot(plotData) +
    geom_bar(aes(x=articleYear,y=n,fill=articleHasKeyword),stat="identity") +
    theme_bw() +
    scale_fill_manual(values=pal,name = "",labels = c("All Other Articles", "New Product Articles")) +
    theme(axis.text.x=element_text(size=14),
          axis.text.y=element_text(size=14),
          axis.title=element_text(size=16),
          title=element_text(size=20),
          legend.position="bottom",
          legend.text = element_text(size=16)) +
    labs(x="Year",y="Number of Articles", title = mains[i])
  plot(p)
  #dev.off()
}


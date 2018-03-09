# Explore tech crunch data
library(dplyr)
library(stringr)
library(reshape2)
library(ggplot2)
library(viridis)
library(scales)
library(lubridate)

# upload cleaned data
scrapedFiles = list.files("./data/business_innovation/working/proquestScrapes", full.names = TRUE)
ncompanies = length(scrapedFiles)
keywords<-c("launch","new product","product release")

activeFile = read.csv(scrapedFiles[1], stringsAsFactors = FALSE)

articleHasKeyword = grepl(paste(keywords, collapse = '|'), activeFile$Full.Text)
articleYear = year(ymd(activeFile$Publication.date))
plotDat = data.frame(articleYear, articleHasKeyword)

pdf("new_prod_articles_year.pdf",width=6,height=5)
ggplot(tech_yrm) +
  geom_bar(aes(x=year,y=value,fill=variable),stat="identity") +
  theme_bw() +
  scale_fill_manual(values=pal,name = "",labels = c("All Other Articles", "New Product Articles")) +
  theme(axis.text.x=element_text(size=14),
        axis.text.y=element_text(size=14),
        axis.title=element_text(size=16),
        title=element_text(size=20),
        legend.position="bottom",
        legend.text = element_text(size=16)) +
  labs(x="Year",y="Number of Articles")
dev.off()


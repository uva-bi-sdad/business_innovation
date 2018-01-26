install.packages("rvest")
library(rvest)

#
# Scrape CIK (company specific codes from the SEC) to SIC code crosswalk
#

url = "https://www.sec.gov/divisions/corpfin/organization/"

secHtml = read_html(paste0(url, "cfia.shtml"))

secHtml %>%
  html_nodes(".center") %>%
  html_children %>%
  html_attr("href") -> links

tableURLs = paste0(url, links)

secTableHtml = tableURLs[1]

findSecTable = function(url){
  url %>%
  read_html %>%
  html_nodes(xpath = "//*[@id=\"cos\"]") %>%
  html_table
}

cikSicCrosswalk = do.call(rbind, sapply(tableURLs, findSecTable))
rownames(cikSicCrosswalk) = NULL

#
# The NAICS to SIC crosswalk wasn't amenable to scraping, but it was easy to just copy paste the table from the page. So we just load it in and do the merge
#

naicsSicCrosswalk = read.csv("./data/business_innovation/original/naicsSicCrosswalk.csv")

cikSicNaics = merge(cikSicCrosswalk, naicsSicCrosswalk, by.x = "SIC Code", by.y = "SIC.Code")
write.csv(cikSicNaics, "./data/business_innovation/working/cikSicNaicsCrosswalk.csv")






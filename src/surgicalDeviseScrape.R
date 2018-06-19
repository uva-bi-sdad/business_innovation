#Code to scrape the medical devices trade journal, surgical products magazines, apparel section
library(RCurl)
library(xml2)
library(stringr)
library(rvest)
library(jsonlite)
# source("./src/eiriki/PharmacyTimes/01_pt_Scrape.R")

# str_subset(a_links, "product-release")
# str_extract(a_links, "product")

# a_links[grep("^(?=.*product-release)(?!.*jpg)", a_links, perl=TRUE)]
url <- 'https://www.surgicalproductsmag.com/product-categories/apparel'

urls <- paste('https://www.surgicalproductsmag.com/product-categories/apparel?page=', i = 1:22, sep = '')
urls <- c(url, urls)

resultsListTitle = vector("list", length(urls))

# for(i  in 1:length(urls)){
#page <-  read_html(urls[i])

#a_links <- html_nodes(page[i], 'a')
#a_links[grep("^(?=.*product-release)(?!.*jpg)", a_links, perl=TRUE)]
# resultsList[[i]] = resultsFromStuff
#}

# extract the titles of the articles

a <- for(i in 1:length(urls)) {

  page <- read_html(urls[i])
  a <- html_nodes(page, 'a')
  # grep("product-release", a, value = TRUE)
  resultsListTitle[[i]] <- grep("^(?=.*product-release)(?!.*jpg)", a, perl=TRUE, value = TRUE)

}

# now extract the body text
for(i in 1:length(urls)) {

}

html_nodes(page, 'p')

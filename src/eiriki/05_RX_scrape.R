#this will scrape all of the issue links on the page for Rx product news
library(RCurl)
library(xml2)
library(stringr)
library(rvest)
library(jsonlite)
source("01_pt_Scrape.R")

#This section will focus on getting all Rx Product news links
i=0
otc_link = c()

#Here, we grab the links to the first 10 pages of Rx product news. The data is missing past there: approx 2007
for(i in 1:11){
  otc_link[i] = paste("http://www.pharmacytimes.com/publications/issue/departments/rx-product-news?p=", i, sep = '')
}

#Now we need to get each page link from the otclinks
master_list = c()
simple_list = c()
for(i in 1:11){
  page <- read_html(otc_link[i])
  simple_list = page %>%         #this gets the link of every otc news article on the current page
    html_nodes('.articleTitle') %>%
    html_nodes('a:nth-child(1)') %>%
    html_attr('href')

  #copy each simplelist entry over to the master list
  for(j in 1:length(simple_list)){
    new_dat <- simple_list[j]
    master_list[length(master_list) + 1] <- simple_list[j]
  }
}
master_list = data.frame(master_list, stringsAsFactors = F)

#now we run the function on every single link provided: the text files for each month are stored separately
for(k in 1:121){
  pt_Rx_scrape(master_list[k,],k)
}

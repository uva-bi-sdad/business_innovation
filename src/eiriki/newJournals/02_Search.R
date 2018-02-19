#This script will focus on retrieving a list of 'New & Approved' article links from Pharmacy Today
library(RCurl)
library(xml2)
library(stringr)
library(rvest)
library(jsonlite)
source("src/eiriki/newJournals/01_pTodayScrape.R")

links = c() #declare array to hold the links in
for (i in 1:3){
  links[i] = paste("http://www.pharmacytoday.org/action/doSearch?searchType=quick&searchText=New+Approval&occurrences=articleTitle&journalCode=ptdy&searchScope=fullSite&contentType=articles&startPage=", i-1, sep = '')
}

#now that we have the links to each page of the search, go through each page and store the links to those articles
simple_list = c()
master_list = c()
for (i in 1:3){
  page <- read_html(links[i])
  simple_list = page %>%         #this gets the link of every otc news article on the current page
    html_nodes('.title') %>%
    html_nodes('a') %>%
    html_attr('href')

  #copy each simplelist entry over to the master list
  for(j in 1:length(simple_list)){
    new_dat <- simple_list[j]
    master_list[length(master_list) + 1] <- simple_list[j]
    #it seems that this master list is not in the correct order right now....
  }
}
master_list = data.frame(master_list, stringsAsFactors = F)

#now we run the function on every single link provided: the text files for each month are stored separately
#sleep for 3 seconds on each call to the function
for(k in 1:59){
  pt_scrape(master_list[k,],k)
  Sys.sleep(20) #make sure to sleep so we don't get blocked from the website
}

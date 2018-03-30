#This script will focus on retrieving a list of 'New & Approved' article links from Pharmacy Today
library(RCurl)
library(xml2)
library(stringr)
library(rvest)
library(jsonlite)
source("src/eiriki/newJournals/01_pTodayScrape.R")

links = c() #declare array to hold the links in
n = 4 #number of pages to get links for
for (i in 1:n){
  links[i] = paste("http://www.pharmacytoday.org/action/doSearch?journalCode=ptdy&searchText1=New+Approval&occurrences1=articleTitle&op1=or&searchText2=New+formulation&occurrences2=articleTitle&op2=or&searchText3=New+indication&occurrences3=articleTitle&catSelect=part&prodVal=HA&date=range&dateRange=&searchAttempt=1482788731&searchType=advanced&doSearch=Search&sortBy=date&startPage=", i-1, sep = '')
}

#now that we have the links to each page of the search, go through each page and store the links to those articles
simple_list = c()
master_list = c()
for (i in 1:n){
  page <- read_html(links[i])
  simple_list = page %>%         #this gets the link of every otc news article on the current page
    html_nodes('.title') %>%
    html_nodes('.rightTitleInfo+ a') %>%
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
for(k in 1:nrow(master_list)){
  pt_scrape(master_list[k,],k)
  Sys.sleep(15) #make sure to sleep so we don't get blocked from the website
}

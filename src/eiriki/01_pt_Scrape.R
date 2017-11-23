#Code to scrape the pharmaceutical Trade Journal, Pharmacy Times
library(RCurl)
library(xml2)
library(stringr)
library(rvest)
library(jsonlite)
#this function takes a pharmacy times link, and also a counter integer n that is meant to help with text file output
pt_scrape <- function(link,n){
  #get the link to scrape from
  new_link <- paste("http://www.pharmacytimes.com", link, sep ="")
  PTLink <- read_html(new_link)

  #Get the entire body of product news text
  pt <- PTLink %>%
    html_node('.orangebt') %>%
    html_text() %>%
    str_trim()

  date <- PTLink %>%
    html_node('.detailPublished') %>%
    html_text() %>%
    str_trim()

  write(pt, file = paste0("./pharmacyTimes_OTC/",n,"PTscrape_", date,".txt"), append = FALSE) #run this line to write monthly data to a text file
}

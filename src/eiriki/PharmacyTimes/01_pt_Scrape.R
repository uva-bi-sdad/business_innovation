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
  date <- PTLink %>%
    html_node('.detailPublished') %>%
    html_text() %>%
    str_trim()

  data <- PTLink %>%
    html_node('.orangebt') %>%
    html_text() %>%
    str_trim()

  #make date recognizable every time
  date = paste0("Date: ",date)
  data = paste(date,data, sep= "\n")


  write(data, file = paste0("./data/business_innovation/working/PHARMACY_TIMES/OTC/",sprintf("%03d", as.numeric(n)) ,"PTscrape_", date,".txt"), append = FALSE) #run this line to write monthly data to a text file
}


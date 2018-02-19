#this script will focus on web scraping the Pharmacy Today website by taking the information from a given article
#and storing it into a text file
library(RCurl)
library(xml2)
library(stringr)
library(rvest)
library(jsonlite)
#this function takes a pharmacy today link, and also a counter integer n that is meant to help with text file output
pt_scrape <- function(link,n){
  #counter here
  n = paste0("00",n)
  #get the link to scrape from
  new_link <- paste("http://www.pharmacytoday.org", link, sep ="")
  PTLink <- read_html(new_link)

  #Get the section title node
  # title <- PTLink %>%
  #   html_nodes('.sectionTitle') %>%
  #   html_text() %>%
  #   str_trim()

  #Get the body text nodes
  #so far this is working out ok, but it would be helpful to attach the title to each section
  bod <- PTLink %>%
    html_nodes('.content') %>%
    html_nodes('.content') %>%
    html_nodes('p') %>%
    html_text() %>%
    str_trim()

   date <- PTLink %>%
     html_node('.artBib') %>%
     html_node('a') %>%
     html_text() %>%
     str_trim()

  write(bod, file = paste0("./src/eiriki/newJournals/Data/",n,"_PharmacyToday_",date,".txt"), append = FALSE) #run this line to write the article to a text file
}

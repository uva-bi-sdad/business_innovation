#Code to scrape the pharmaceutical Trade Journal, Pharmacy Times
library(RCurl)
library(xml2)
library(stringr)
library(rvest)
library(jsonlite)

pt_scrape <- function(link){
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

  write(pt, file = paste0("./pharmacyTimes/PTscrape_", date,".txt"), append = FALSE) #appending because we want all data for one year in a single txt file
}

#this script will do bolded words for pharmacy times
#Code to scrape the pharmaceutical Trade Journal, Pharmacy Times
library(RCurl)
library(xml2)
library(stringr)
library(rvest)
library(jsonlite)

#this function takes a pharmacy times link, and also a counter integer n that is meant to help with text file output
#pt_RX_scrape <- function(link,n){
  #get the link to scrape from
  new_link <- "http://www.pharmacytimes.com/publications/issue/2018/february2018/rx-product-news-february-2018"
  PTLink <- read_html(new_link)

  #Get the entire body of product news text
  date <- PTLink %>%
    html_node('.detailPublished') %>%
    html_text() %>%
    str_trim()

  dat <- PTLink %>%
    html_node('.orangebt') %>%
    xml_contents()

  i =1
  bod = c() #this while loop goes through the body contents, and if we see "manufactured", get the company on next line
  while(i < length(dat)){
    if(str_detect(str_to_lower(dat[i]), "manufactured")){
      bod[i]=html_text(dat[i])
      bod[i+1]=html_text(dat[i+1])
      i = i +2
      next()
    }
    i = i +1
  }
  bod = bod[complete.cases(bod)]

  data = c(1:(length(bod)+1)) #empty frame to hold date first, then strings in body
  frame_date = paste0("Date: ",date)
  data[1] = frame_date #first entry is date
  data[2:length(data)] = bod

  #write(data, file = paste0("./data/business_innovation/working/PHARMACY_TIMES/Rx/",sprintf("%03d", as.numeric(n)) ,"RXscrape_", date,".txt"), append = FALSE) #run this line to write monthly data to a text file
#}

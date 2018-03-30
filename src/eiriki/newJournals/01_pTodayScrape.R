#this script will focus on web scraping the Pharmacy Today website by taking the information from a given article
#and storing it into a text file
library(RCurl)
library(xml2)
library(stringr)
library(rvest)
library(jsonlite)
#this function takes a pharmacy today link, and also a counter integer n that is meant to help with text file output
pt_scrape <- function(link,n){
  #counter n
  #get the link to scrape from
  new_link <- paste("http://www.pharmacytoday.org", link, sep ="")
  PTLink <- read_html(new_link)

  #Get the section title node
  # title <- PTLink %>%
  #   html_nodes('.sectionTitle') %>%
  #   html_text() %>%
  #   str_trim()

  #Get the body text nodes
  bod <- PTLink %>%
    html_nodes('.content') %>% #one level of content down so we don't get junk at end of file
    html_nodes('#artTabContent li , section,p') %>%
    html_nodes('strong, h2,h1') %>%  #this line here grabs all the bolded words from the text body and headers
    html_text() %>%
    str_trim()

  if(length(bod) == 0 ){ #for articles on Pharmacy today older than 2014
    bod <- PTLink %>%
      html_nodes('.content') %>% #one level of content down so we don't get junk at end of file
      html_nodes('p , #article .articleTitle') %>%
      html_nodes('strong, h1') %>%  #this line here grabs all the bolded words from the text body and headers
      html_text() %>%
      str_trim()
  }

   date <- PTLink %>%
     html_node('.artBib') %>%
     html_node('a') %>%
     html_text() %>%
     str_trim()

   #make date recognizable every time
   data = c(1:(length(bod)+1)) #empty frame to hold date first, then strings in body
   frame_date = paste0("Date: ",date)
   data[1] = frame_date #first entry is date
   data[2:length(data)] = bod
  write(data, file = paste0("./data/business_innovation/working/PHARMACY_TODAY/",sprintf("%03d", as.numeric(n)) ,"_PharmacyToday_", date,".txt"), append = FALSE) #run this line to write the article to a text file
}

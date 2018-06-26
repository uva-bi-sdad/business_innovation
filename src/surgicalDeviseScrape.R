#Code to scrape the medical devices trade journal, surgical products magazines, apparel section
library(RCurl)
library(xml2)
library(stringr)
library(rvest)
library(jsonlite)
# a_links[grep("^(?=.*product-release)(?!.*jpg)", a_links, perl=TRUE)]

categories <- c('APPAREL',
'Lighting-OR-Visualization-Diagnostics',
'OR Equipment-Accessories',
'BARIATRIC SURGICAL PRODUCTS',
'ANESTHESIOLOGY',
'CRITICAL-CAREEMERGENCY',
'ELECTROSURGERY-PRODUCTS',
'INFECTION-CONTROL-PRODUCTS',
'INFORMATION-COMMUNICATION-SYSTEMS',
'INSTRUMENTATION',
'MINIMALLY-INVASIVE-SURGERY',
'MONITORING-PRODUCTS',
'OBGYN-PRODUCTS',
'ORTHOPEDICSARTHROSCOPY-PRODUCTS',
'SAFETY-PRODUCTS',
'SERVICES-TRAINING',
'WOUND-CLOSUREHEALING-PRODUCTS')

base_url <- 'https://www.surgicalproductsmag.com/product-categories/'
categories_url <- paste(base_url, categories, sep = "")


url <- 'https://www.surgicalproductsmag.com/product-categories/apparel'
urls <- str_c('https://www.surgicalproductsmag.com/product-categories/apparel?page=', 1:22)
urls <- c(url, urls)

results = vector("list", length(urls))

# for(i  in 1:length(urls)){
#page <-  read_html(urls[i])

#a_links <- html_nodes(page[i], 'a')
#a_links[grep("^(?=.*product-release)(?!.*jpg)", a_links, perl=TRUE)]
# resultsList[[i]] = resultsFromStuff
#}

# extract the titles of the articles

# while extract the titles, also extract the hrefs of each link

# a <- for(i in 1:length(urls)) {
# i = 1L
  # page <- read_html(urls[i])
  # a <- html_nodes(page, 'a') %>%
    # html_attr('href')
    # as.character()
  # resultsListTitle[[i]] = a %>%
    # subset(str_detect(string = a,
      #                pattern = "product-release/\\d{4}")) %>%
    # str_extract(pattern = '(?<=>)\\w.*(?=\\s*</a>)') %>%
    # na.omit()
  # }

# this loop goes through the pages and grabs the link to each articles
a <- for(i in 1:length(urls)) {
    page <- read_html(urls[i])
    a <- html_nodes(page, '.smaller-heading a') %>%
    html_attr('href')
    results[[i]] = a
}

results

# setting up the function

scrape_surgical <- function(link,n){
  new_link <- paste("https://www.surgicalproductsmag.com", link, sep ="")
  PTLink <- read_html(new_link)

  #Get the entire body of product news text
  date <- PTLink %>%
    html_node('.views-field-created .field-content') %>%
    html_text() %>%
    str_trim()

  title <- PTLink %>%
    html_node('h1') %>%
    html_text() %>%
    str_trim()

  body <- PTLink %>%
    html_nodes('#block-system-main p') %>%
    html_text() %>%
    str_trim %>%
    paste(collapse = " ")

  return(surgical_df <- data.frame(date = date,
                        title = title,
                        body = body))

}

c <- scrape_surgical(results[[2]][1])

results <- unlist(results)

d <- lapply(results, scrape_surgical) # outputs a list of dataframe

# lapply on them, and use do.call
g <- do.call(rbind, d)

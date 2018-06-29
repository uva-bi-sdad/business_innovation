#Code to scrape the medical devices trade journal, surgical products magazines, apparel section
library(RCurl)
library(xml2)
library(stringr)
library(rvest)
library(jsonlite)
library(dplyr)
# a_links[grep("^(?=.*product-release)(?!.*jpg)", a_links, perl=TRUE)]

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
    html_node('.views-field-title .field-content') %>%
    html_text() %>%
    str_trim()

  body <- PTLink %>%
    html_nodes('#block-system-main p') %>%
    html_text() %>%
    str_trim %>%
    paste(collapse = " ") %>%
    str_trim()

  return(surgical_df <- data.frame(date = date,
                        title = title,
                        body = body))

}

c <- scrape_surgical(results[[2]][1])

results <- unlist(results)

d <- lapply(results, scrape_surgical) # outputs a list of dataframe

# lapply on them, and use do.call
g <- do.call(rbind, d)

# adding a new variable, cateories, to the existing dataframe
g$categories <- "apparel"
g <- g[4:91,]

## scraping functions for lighting/visualization/diagnositics
# for 2013, 2014 and 2015, count how many products are in each company
# make bar charts of the companies mentioned
# first word of every article, put in the a separate column

### Lighting and visualization

urlLighting <- 'https://www.surgicalproductsmag.com/product-categories/lighting-or-visualization-diagnostics'
urlsLighting <- str_c('https://www.surgicalproductsmag.com/product-categories/lighting-or-visualization-diagnostics/?page=', 1:35)
urlsLighting <- c(urlLighting, urlsLighting)

resultsLighting = vector("list", length(urlsLighting))

aLighting <- for(i in 1:length(urlsLighting)) {
  page <- read_html(urlsLighting[i])
  aLighting <- html_nodes(page, '.smaller-heading a') %>%
    html_attr('href')
  resultsLighting[[i]] = aLighting
}
lightingList <- unlist(resultsLighting)

dLighting <- lapply(lightingList, scrape_surgical)
dfLighting <- do.call(rbind, dLighting)
dfLighting$categories <- "Lighting/Visualization/Diagnostics"
dfLighting <- dfLighting[3:141,]

### OR EQUIPMENT AND ACCESSORIES
urlsOr <- str_c('https://www.surgicalproductsmag.com/product-categories/or-equipment-accessories?page=', 0:60)
resultsOr <- vector("list", length(urlsOr))
aOr <- for(i in 1:length(urlsOr)) {
  page <- read_html(urlsOr[i])
  aOr <- html_nodes(page, '.smaller-heading a') %>%
    html_attr('href')
  resultsOr[[i]] = aOr
}

orList <- unlist(resultsOr)
dOr <- lapply(orList, scrape_surgical)
dfOr <- do.call(rbind, dOr)
dfOr$categories <- "Or Equipment and Accessories"
dfOr <- dfOr[5:178,]

## putting the dataframe together
df <- rbind(g, dfLighting)
df <- rbind(df, dfOr)

## Bariatric SURGICAL PRODUCTS
urlsBar <- str_c('https://www.surgicalproductsmag.com/product-categories/bariatric-surgical-products?page=', 0:3)
resultsBar <- vector("list", length(urlsBar))
aBar <- for(i in 1:length(urlsBar)) {
  page <- read_html(urlsBar[i])
  aBar <- html_nodes(page, '.smaller-heading a') %>%
    html_attr('href')
  resultsBar[[i]] = aBar
}

barList <- unlist(resultsBar)
dBar <- lapply(barList, scrape_surgical)
dfBar <- do.call(rbind, dBar)
dfBar <- dfBar[1:9,]
dfBar$categories <- "Barriatric surgical products"

## ANESTHESIOLOGY
urlsAnes <- str_c('https://www.surgicalproductsmag.com/product-categories/anesthesiology?page=',0:1)
resultsAnes <- vector("list", length(urlAnes))
aAnes <- for(i in 1:length(urlsAnes)) {
  page <- read_html(urlsAnes[i])
  aAnes <- html_nodes(page, '.smaller-heading a') %>%
    html_attr('href')
  resultsAnes[[i]] = aAnes
}
anesList <- unlist(resultsAnes)
dAnes <- lapply(anesList, scrape_surgical)
dfAnes <- do.call(rbind, dAnes)
dfAnes <- df[1:3,]
dfAnes$categories <- "Anesthesiology"

##CRITICAL CARE/EMERGENCY
urlCrit <- str_c('https://www.surgicalproductsmag.com/product-categories/critical-careemergency?page=',0:4)
resultsCrit <- vector("list", length(urlCrit))
resultsAnes <- vector("list", length(urlAnes))
aCrit <- for(i in 1:length(urlCrit)) {
  page <- read_html(urlCrit[i])
  aCrit <- html_nodes(page, '.smaller-heading a') %>%
    html_attr('href')
  resultsCrit[[i]] = aCrit
}
critList <- unlist(resultsCrit)
dCrit <- lapply(critList, scrape_surgical)
dfCrit <- do.call(rbind, dCrit)
dfCrit <- dfCrit[1:12,]
dfCrit$categories <- "critical care/emergency"

# ELECTROSURGERY PRODUCTS
urlEle <- str_c('https://www.surgicalproductsmag.com/product-categories/electrosurgery-products?page=',0:7)
resultsEle <- vector("list", length(urlEle))
aEle<- for(i in 1:length(urlEle)) {
  page <- read_html(urlEle[i])
  aEle <- html_nodes(page, '.smaller-heading a') %>%
    html_attr('href')
  resultsEle[[i]] = aEle
}
eleList <- unlist(resultsEle)
dEle <- lapply(eleList, scrape_surgical)
dfEle <- do.call(rbind, dEle)
dfEle <- dfEle[2:30,]
dfEle$categories <- "electrosurgery products"

### INFECTION CONTROL PRODUCTS
urlInf <- str_c('https://www.surgicalproductsmag.com/product-categories/infection-control-products?page=',0:28)
resultsInf <- vector("list", length(urlInf))
aInf <- for(i in 1:length(urlInf)) {
  page <- read_html(urlInf[i])
  aInf <- html_nodes(page, '.smaller-heading a') %>%
    html_attr('href')
  resultsInf[[i]] = aInf
}
infList <- unlist(resultsInf)
dInf <- lapply(infList, scrape_surgical)
dfInf <- do.call(rbind, dInf)
dfInf <- dfInf[5:165,]
dfInf$categories <- "Infection Control Products"

### INFORMATION & COMMUNICATION SYSTEM
urlInfo <- str_c('https://www.surgicalproductsmag.com/product-categories/information-communication-systems?page=', 0:14)
resultsInfo <- vector("list", length(urlInfo))
aInfo <- for(i in 1:length(urlInfo)) {
  page <- read_html(urlInfo[i])
  aInfo <- html_nodes(page, '.smaller-heading a') %>%
    html_attr('href')
  resultsInfo[[i]] = aInfo
}
infoList <- unlist(resultsInfo)
dInfo <- lapply(infoList, scrape_surgical)
dfInfo <- do.call(rbind, dInfo)
dfInfo <- dfInfo[1:34,]
dfInfo$categories <- "information & communication system"

## INSTRUMENTATION
urlInstr <- str_c('https://www.surgicalproductsmag.com/product-categories/instrumentation?page=', 0:12)
resultsInstr <- vector("list", length(urlInstr))
aInstr <- for(i in 1:length(urlInstr)) {
  page <- read_html(urlInstr[i])
  aInstr <- html_nodes(page, '.smaller-heading a') %>%
    html_attr('href')
  resultsInstr[[i]] = aInstr
}
instrList <- unlist(resultsInstr)
dInstr <- lapply(instrList, scrape_surgical)
dfInstr <- do.call(rbind, dInstr)
dfInstr <- dfInstr[1:51,]
dfInstr$categories = "Instrumentation"

## MINIMALLY INVASIVE SURGERY
urlMini <- str_c('https://www.surgicalproductsmag.com/product-categories/minimally-invasive-surgery?page=',0:23)
resultsMini <- vector("list", length(urlMini))
aMini <- for(i in 1:length(urlMini)) {
  page <- read_html(urlMini[i])
  aMini <- html_nodes(page, '.smaller-heading a') %>%
    html_attr('href')
  resultsMini[[i]] = aMini
}
miniList <- unlist(resultsMini)
dMini <- lapply(miniList, scrape_surgical)
dfMini <- do.call(rbind, dMini)
dfMini <- dfMini[4:52,]
dfMini$categories <- "Minimally Invasive Surgery"

### MONITORING PRODUCTS
urlMon <- str_c('https://www.surgicalproductsmag.com/product-categories/monitoring-products?page=',0:8)
resultsMon <- vector("list", length(urlMon))
aMon <- for(i in 1:length(urlMon)) {
  page <- read_html(urlMon[i])
  aMon <- html_nodes(page, '.smaller-heading a') %>%
    html_attr('href')
  resultsMon[[i]] = aMon
}
monList <- unlist(resultsMon)
dMon <- lapply(monList, scrape_surgical)
dfMon <- do.call(rbind, dMon)
dfMon <- dfMon[1:11,]
dfMon$categories <- "Monitoring products"

##OBGYM PRODUCTS
urlOb <- str_c('https://www.surgicalproductsmag.com/product-categories/obgyn-products?page=',0:4)
resultsOb <- vector("list", length(urlOb))
aOb <- for(i in 1:length(urlOb)) {
  page <- read_html(urlOb[i])
  aOb <- html_nodes(page, '.smaller-heading a') %>%
    html_attr('href')
  resultsOb[[i]] = aOb
}
obList <- unlist(resultsOb)
dOb <- lapply(obList, scrape_surgical)
dfOb <- do.call(rbind, dOb)
dfOb <- dfMini[1:5,]
dfOb$categories <- "OBGYN Products"

### ORTHEPEDICTS/ARTHROSCOPY PRODUCTS
urlOrth <- str_c('https://www.surgicalproductsmag.com/product-categories/orthopedicsarthroscopy-products?page=',0:14)
resultsOrth <- vector("list", length(urlOrth))
aOrth <- for(i in 1:length(urlOrth)) {
  page <- read_html(urlOrth[i])
  aOrth <- html_nodes(page, '.smaller-heading a') %>%
    html_attr('href')
  resultsOrth[[i]] = aOrth
}
orthList <- unlist(resultsOrth)
dOrth <- lapply(orthList, scrape_surgical)
dfOrth <- do.call(rbind, dOrth)
dfOrth <- dfOrth[2:50,]
dfOrth$categories <- "Orthepedics/Arthroscopy Products"

### SAFETY PRODUCTS
urlSafe <- str_c('https://www.surgicalproductsmag.com/product-categories/safety-products?page=',0:7)
resultsSafe <- vector("list", length(urlSafe))
aSafe <- for(i in 1:length(urlSafe)) {
  page <- read_html(urlSafe[i])
  aSafe <- html_nodes(page, '.smaller-heading a') %>%
    html_attr('href')
  resultsSafe[[i]] = aSafe
}
safeList <- unlist(resultsSafe)
dSafe <- lapply(safeList, scrape_surgical)
dfSafe <- do.call(rbind, dSafe)
dfSafe <- dfSafe[2:37,]
dfSafe$categories <- "Safety Products"

## service training
urlSer <- str_c('https://www.surgicalproductsmag.com/product-categories/services-training?page=',0:6)
resultsSer <- vector("list", length(urlSer))
aSer<- for(i in 1:length(urlSer)) {
  page <- read_html(urlSer[i])
  aSer <- html_nodes(page, '.smaller-heading a') %>%
    html_attr('href')
  resultsSer[[i]] = aSer
}
serList <- unlist(resultsSer)
dSer <- lapply(serList, scrape_surgical)
dfSer <- do.call(rbind, dSer)
dfSer <- dfSer[1:15,]
dfSer$categories <- "Service Training"

###wound closure
urlWound <- str_c('https://www.surgicalproductsmag.com/product-categories/wound-closurehealing-products?page=', 0:19)
resultsWound <- vector("list", length(urlWound))
aWound<- for(i in 1:length(urlWound)) {
  page <- read_html(urlWound[i])
  aWound <- html_nodes(page, '.smaller-heading a') %>%
    html_attr('href')
  resultsWound[[i]] = aWound
}
woundList <- unlist(resultsWound)
dWound<- lapply(woundList, scrape_surgical)
dfWound <- do.call(rbind, dWound)
dfWound <- dfWound[2:34,]
dfWound$categories <- "Wound Closure"

surgical <- rbind(dfBar, dfCrit)
surgical <- rbind(surgical, dfEle)
surgical <- rbind(surgical, dfInf)
surgical <- rbind(surgical, dfInfo)
surgical <- rbind(surgical, dfInstr)
surgical <- rbind(surgical, dfLighting)
surgical <- rbind(surgical, dfMini)
surgical <- rbind(surgical, dfMon)
surgical <- rbind(surgical, dfOb)
surgical <- rbind(surgical, dfOr)
surgical <- rbind(surgical, dfOrth)
surgical <- rbind(surgical, dfSafe)
surgical <- rbind(surgical, dfSer)
surgical <- rbind(surgical, dfWound)

dim(surgical)
write.csv(surgical, 'surgical.csv', row.names = FALSE)

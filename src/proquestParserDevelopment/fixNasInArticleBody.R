# library(rvest)
# library(stringr)
# library(data.table)
# library(lubridate)
# library(zoo)
#
# # These set the filename of the html file we're looking at, as well as the names of the fields we want to extract.
# proQuestHtml = "./data/business_innovation/original/scrapedProquestData/novartis1.html"
# fieldsOfInterest = c("Subject", "Company", "Publication title", "Publication date", "Publication subject", "Source type", "Document type")
#
# # What follows are the guts of the parseProquest function. I've spilled them here so you can more easily tool around with them.
#
# pfizerData = read_html(proQuestHtml)
# divs = html_nodes(pfizerData, "body > div")
# nArticles = length(divs) - 1 # the last div is for a copyright thing
# p = length(fieldsOfInterest)
#
# # Here, i sets the particular article you're looking at in the list of divs. I suggest looking at the first to see what it does, then go find some articles where the body text didn't parse and see if you can fix it.
#
# i = 127
#
# # Article title
#
# html_nodes(divs[i], "p") %>%
#   html_attr("style") %>%
#   grep("bold", .) -> titleParagraph
# (html_nodes(divs[i], "p") %>% html_text)[titleParagraph]
#
# # Other fields
#
# # The period here is a dplyr thing. By defauly, the pipe f(a) %>% g() puts the result of f(a) into the first argument of g. If you want f(a) to go somewhere else, you can use a period to denote where you want the output from the pipe to go. If you look at all the dlyr functions, the 'grammatical object' of the function always is in the first argument. This means it's almost never necessary to use periods with dplyr functions, but as grep is a base R function I had to do that.
#
# # 'strong' here means it appears as a bold text. I noticed all the 'section names' seemed to be bolded, so I used that to filter.
# otherFields = html_nodes(divs[i], "p") %>% '['(grep("strong", .)) %>% html_text
#
# # Loop through and try to pull out all the fields besides the full text and title.
# for(j in 1:p){
#   fieldText = otherFields[grep(fieldsOfInterest[j], str_extract(otherFields, ".{1,30}"))[1]]
#   print(str_extract(fieldText, "(?<=: )(.*)"))
# }
#
# # Full text. Here's where the action is. Have fun.
# fullTextParagraphs = html_nodes(divs[i], "p") %>% html_attr("style") %>% is.na
#
# #simple conditional handling NA
# #if we see there is no full text (no nodes with NA style)
# #grab the abstract?
# if(!all(is.na(fullTextParagraphs))){
#   abstract_vec <- str_detect(as.character(xml_children(divs[i])),'Abstract') #vector for where string 'abstract' is true
#   truth_idx <- which(abstract_vec) #what index is it
#
#   #abstract_vec[truth_idx] = FALSE   #abstract text is in next child so reset truth values
#   #abstract_vec[truth_idx+1] = TRUE
#
#   #assign to full text variable
#   fullTextParagraphs = abstract_vec
# }
#
# txt = paste((html_nodes(divs[i], "p") %>% html_text)[fullTextParagraphs], collapse = '')
# txt = gsub("\\n", "", txt) # This gets rid of newline characters
# txt = str_extract(txt, ".{1,5000}") #This regex selects the first 5000 characters. I was running into space limits in excel, so I did this.
# is.na(txt)
# txt





#########################CHECK
library(zoo)

parseProquest = function(proQuestHtml, fieldsOfInterest, reportEach = 100){

  pfizerData = read_html(proQuestHtml)
  divs = html_nodes(pfizerData, "body > div")
  n = length(divs) - 1 # the last div is for a copyright thing
  p = length(fieldsOfInterest)

  out = matrix(NA, nrow = n, ncol = p + 2)
  colnames(out) = c("Article Title", fieldsOfInterest, "Full Text")
  out = data.frame(out)

  for(i in 1:n){

    # Article title

    html_nodes(divs[i], "p") %>%
      html_attr("style") %>%
      grep("bold", .) -> titleParagraph
    out[i, 1] = (html_nodes(divs[i], "p") %>% html_text)[titleParagraph]

    # Other fields

    otherFields = html_nodes(divs[i], "p") %>% '['(grep("strong", .)) %>% html_text
    for(j in 1:p){
      txt = otherFields[grep(fieldsOfInterest[j], str_extract(otherFields, ".{1,30}"))[1]]
      out[i, j + 1] = str_extract(txt, "(?<=: )(.*)")
    }

    # Full text
    fullTextParagraphs = html_nodes(divs[i], "p") %>% html_attr("style") %>% is.na
    txt = paste((html_nodes(divs[i], "p") %>% html_text)[fullTextParagraphs], collapse = '')
    txt = gsub("\\n", "", txt)
    txt = str_extract(txt, ".{1,5000}")
    out[i, p + 2] = txt

    if((i %% reportEach) == 0) print(sprintf("Just processed record %s!", i))
  }
  return(out)
}

parseList = function(fileNames, fieldsOfInterest, reportEach = 100){
  outList = vector("list", length(fileNames))
  for(i in 1:length(fileNames)){
    print(paste0("Parsing file ", i))
    outList[[i]] = parseProquest(fileNames[[i]], fieldsOfInterest, reportEach = reportEach)
  }
  out = do.call(rbind, outList)
  out$Publication.date = as.Date(as.yearmon(paste(str_extract(out$Publication.date, "[[:alpha:]]{3}"), str_extract(out$Publication.date, "\\d{4}"))))
  out = data.table(out)
  #out = unique(out, by = "Full.Text")
  #out = unique(out, by = "Article.Title")
  return(out)
}

library(rvest)
library(stringr)
library(data.table)
library(lubridate)
library(zoo)
source("./R/proquestParseFunctions.R")

# messing around
proQuestHtml = "./data/business_innovation/original/scrapedProquestData/pfizer1.html"
fieldsOfInterest = c("Subject", "Company", "Publication title", "Publication date", "Publication subject", "Source type", "Document type")


fileNames = list.files("./data/business_innovation/original/scrapedProquestData", full.names = TRUE)
fieldsOfInterest = c("Subject", "Company", "Publication title", "Publication date", "Publication subject", "Source type", "Document type")
parsedDataOutdir = "./data/business_innovation/working/parsedProquestData/"


# Parse pfizer
pfizerNames = grep("pfizer", fileNames, value = TRUE)
parsedPfizer = parseList(pfizerNames, fieldsOfInterest, reportEach = 100)

sum(is.na(parsedPfizer$Full.Text))
#where are them dang NAs
temp <- dplyr::filter(parsednovartis,is.na(parsednovartis$Full.Text))
View(temp)

#92 NA for regular, try with new function

parseProquest = function(proQuestHtml, fieldsOfInterest, reportEach = 100){

  pfizerData = read_html(proQuestHtml)
  divs = html_nodes(pfizerData, "body > div")
  n = length(divs) - 1 # the last div is for a copyright thing
  p = length(fieldsOfInterest)

  out = matrix(NA, nrow = n, ncol = p + 2)
  colnames(out) = c("Article Title", fieldsOfInterest, "Full Text")
  out = data.frame(out)

  for(i in 1:n){

    # Article title

    html_nodes(divs[i], "p") %>%
      html_attr("style") %>%
      grep("bold", .) -> titleParagraph
    out[i, 1] = (html_nodes(divs[i], "p") %>% html_text)[titleParagraph]

    # Other fields

    otherFields = html_nodes(divs[i], "p") %>% '['(grep("strong", .)) %>% html_text
    for(j in 1:p){
      txt = otherFields[grep(fieldsOfInterest[j], str_extract(otherFields, ".{1,30}"))[1]]
      out[i, j + 1] = str_extract(txt, "(?<=: )(.*)")
    }

    # Full text
    fullTextParagraphs = html_nodes(divs[i], "p") %>% html_attr("style") %>% is.na

    #simple conditional handling NA
    #if we see there is no full text (no nodes with NA style)
    #grab the abstract?
    if(!all(is.na(fullTextParagraphs))){
      abstract_vec <- str_detect(as.character(xml_children(divs[i])),'Abstract') #vector for where string 'abstract' is true
      truth_idx <- which(abstract_vec) #what index is it

      #assign to full text variable
      fullTextParagraphs = abstract_vec
    }
    txt = paste((html_nodes(divs[i], "p") %>% html_text)[fullTextParagraphs], collapse = '')
    txt = gsub("\\n", "", txt)
    txt = str_extract(txt, ".{1,5000}")
    out[i, p + 2] = txt

    if((i %% reportEach) == 0) print(sprintf("Just processed record %s!", i))
  }
  return(out)
}
parsedPfizer = parseList(pfizerNames, fieldsOfInterest, reportEach = 100)

sum(is.na(parsedPfizer$Full.Text))





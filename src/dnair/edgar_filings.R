## LIBRARIES
library(xml2)
library(rvest)
library(stringr)
library(hunspell)
library(data.table)
library(dplyr)

## GRAB ALL PATHS
paths_file <- "data/business_innovation/original/edgar_filings/ALL_SEC_files.txt"
file_headers <- readr::read_tsv(paths_file, col_names = FALSE)
paths <- paste0("data/business_innovation/original/edgar_filings/Edgar_filings_folders/", file_headers$X1)
file_names <- list.files(paths, full.names = TRUE)

## ONE EXAMPLE FILE FOR NOW
txtfile <- file_names[5]
pattern <- "<DOCUMENT>\n<TYPE>GRAPHIC.*?</DOCUMENT>"
pattern2 <- "<DOCUMENT>\n<TYPE>EXCEL.*?</DOCUMENT>"

#TESTING STUFF
patterndoc <- "<DOCUMENT>\n<TYPE>GRAPHIC.*?<\\/DOCUMENT>"
#edgartxt <- readr::read_file(txtfile)
edgar_txt <- stringr::str_remove_all(edgartxt, patterndoc)
nchar(edgar_txt) - nchar(edgartxt)

#junk
#edgar_txt <- stringr::str_remove(edgartxt, pattern)
#edgar_txt <- stringr::str_remove(edgar_txt, pattern2)

edgartxt <- read_html(txtfile)
edgar <- htmltools::HTML(as.character(edgartxt))
edgar <- read_html(edgar)

## LOOKING FOR COMPANY NAME
metadata <- xml_find_all(edgar, ".//b")
date <- html_text(metadata[7])
company <- html_text(metadata[13])

## PULL ALL DIVS,
div <- xml_find_all(edgar, ".//p")
div_text <- html_text(div)
div_text_clean <- str_squish(div_text)
div_text_clean_20plus <- subset(div_text_clean, nchar(div_text_clean) >= 20) #take SEC filing texts that have at least 20 characters

word_innov<- c("launch", "new product")
innov_text <- which(grepl(paste(word_innov, collapse = "|"), tolower(div_text_clean_20plus)) == TRUE)

words <- unlist(stringr::str_split(gsub('[[:punct:] ]+',' ', div_text_clean_20plus[innov_text]), " ", simplify = FALSE))
words <- words %>%
  str_replace(pattern = "\u0092", "") %>%
  str_replace(pattern = "\u0093", "") %>%
  str_replace(pattern = "\u0094", "") %>%
  str_replace(pattern = "\u0095", "") %>%
  str_replace(pattern = "\u0097", "")
eng <- hunspell_check(words, dict = dictionary("en_US"))
caps <- grepl("^[[:upper:]]", words)

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

example2 <- tibble::tibble("Word" = words,
                           "Capitals" = caps,
                           "English" = eng) %>%
  filter(English == FALSE & Capitals == TRUE & nchar(Word)>3 & substrRight(Word, 1) != "s") %>%
  group_by(Word) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  data.table::as.data.table()

innov_text
example2

#write.csv(example2, file = "data/business_innovation/working/dn_table.csv")



## LIBRARIES
library(xml2)
library(rvest)
library(stringr)
library(hunspell)
library(data.table)
library(dplyr)
library(htmltools)
library(magrittr)

## GRAB ALL PATHS
paths_file <- "data/business_innovation/original/edgar_filings/ALL_SEC_files.txt"
file_headers <- readr::read_tsv(paths_file, col_names = FALSE)
paths <- paste0("./data/business_innovation/original/edgar_filings/Edgar_filings_folders/", file_headers$X1)
paths[9]
file_names <- list.files(head(paths), full.names = TRUE)
file_names[9]
head(file_names)

## CREATE FUNCTION
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

## for loop

for (i in file_names[8]) {
  edgar <- read_html(i)
  edgar <- edgar %>%
    as.character() %>%
    HTML() %>%
    read_html()

  metadata <- xml_find_all(edgar, ".//b")
  date <- html_text(metadata[7])
  company <- html_text(metadata[13])
  patt <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")
  patt2 <-as.character(c(2010:2019))
  month <- stringr::str_which(date, patt)
  year <- patt2[stringr::str_which(date, patt2)]

  div <- xml_find_all(edgar, ".//p")
  div_text <- div %>%
    html_text() %>%
    str_squish

  paragraphs <- subset(div_text, nchar(div_text) >= 20)
  word_innov <- c("launch", "new product")

  innov_text <- which(grepl(paste(word_innov, collapse = "|"), tolower(paragraphs)) == TRUE)

  words <- unlist(stringr::str_split(gsub('[[:punct:] ]+',' ', paragraphs[innov_text]), " ", simplify = FALSE))
  words <- words %>%
    str_replace(pattern = "\u0092", "") %>%
    str_replace(pattern = "\u0093", "") %>%
    str_replace(pattern = "\u0094", "") %>%
    str_replace(pattern = "\u0095", "") %>%
    str_replace(pattern = "\u0097", "")
  eng <- hunspell_check(words, dict = dictionary("en_US"))
  caps <- grepl("^[[:upper:]]", words)

  output1 <- tibble::tibble(
    "Company" = company,
    "Month" = month,
    "Year" = year,
    "Words" = words,
    "English" = eng,
    "Capitals" = caps
  )
  output2 <- output1 %>%
    filter(English == FALSE & Capitals == TRUE & nchar(Words)>3 & substrRight(Words, 1) != "s") %>%
    group_by(Words) %>%
    summarise(count = n()) %>%
    arrange(desc(count)) %>%
    data.table::as.data.table()

  print(output1)
  print(output2)

}


#write.csv(example2, file = "data/business_innovation/working/dn_table.csv")





## LIBRARIES

remove_doc_types <- function(xml_string, types = c("GRAPHIC", "EXCEL", "ZIP", "EX-10.3", "EX-10.6", "EX-10.20")) {
  no_ns <- gsub("\\n", " ", xml_string)
  #browser()
  for (t in types) {
    find_str <- paste0("<DOCUMENT> ?<TYPE> ?", t)
    search_str <- paste0("<DOCUMENT> ?<TYPE> ?", t, ".*?</DOCUMENT>")
    found <-
      as.data.table(stringr::str_locate_all(no_ns, find_str))

    for (i in 1:nrow(found)) {
      locs <- as.data.table(stringr::str_locate(no_ns, search_str))
      st <- locs[1, start] - 1
      en <- locs[1, end] + 1
      ifelse(is.na(locs$start) == TRUE & is.na(locs$end) == TRUE, no_ns,
             no_ns <- paste0(substr(no_ns, 1, st), substr(no_ns, en, nchar(no_ns))) )
    }
  }
  no_ns
}


library(xml2)
library(rvest)
library(stringr)
library(hunspell)
library(data.table)
library(dplyr)
library(htmltools)
library(magrittr)
library(htmltidy)
library(readr)

##GRAB METADATA
#cik_ticker <- read_delim("data/business_innovation/original/edgar_filings/cik_ticker.csv", "|", escape_double = FALSE, trim_ws = TRUE)
#head(cik_ticker)

## GRAB ALL PATHS
paths_file <- "data/business_innovation/original/edgar_filings/ALL_SEC_files.txt"
file_headers <- readr::read_tsv(paths_file, col_names = FALSE)
paths <- paste0("data/business_innovation/original/edgar_filings/Edgar_filings_folders/", file_headers$X1)
file_names <- unique(list.files(paths, full.names = TRUE))
paths[9]
file_names[9]
head(file_names)

## remove final data.table if exists
remove(checkthese, date, div, edgar, cleaned, company, dashpatt, div_text, f, i, month, paragraphs, registered, regpatt, regwords1, testpatt, unclean, year, engwords, engpatt, o1, output1)
if (exists("fin_o") == TRUE) rm(fin_o)

# Loop over file paths. For safety, I specify subsets (e.g. file_names[1:1000]) and
# then write each to a file, combining all files at the end
# use trycatch to skip errors
for (i in file_names[c(1:5,8)]) { # 2867
  tryCatch({
    unclean <- read_file(i)
    cleaned <- remove_doc_types(unclean)
    edgar <- read_html(cleaned)
    edgar <- edgar %>%
      as.character() %>%
      HTML() %>%
      read_html()
    print(paste(i))
  }, warning = function(war) {
    print(paste(i, "MY_WARNING:  ", war))
    return(NA)

  }, error = function(err) {
    print(paste(i, "ERROR:  ", err))
    #o <- out.matrix[i, 1] <- paste(basename(repo), "ERROR")
    return(NA)

  }, finally = {
    company <- str_match(basename(i), "(^.*?)_")[, 2]
    date <- str_match(basename(i), "([0-9][0-9][0-9][0-9])-([0-9][0-9])-[0-9][0-9]")
    month <- date[, 3]
    year <- date[, 2]

    div <- xml_find_all(edgar, ".//p")
    div_text <- div %>% #thing3
      html_text() %>%
      str_squish

    paragraphs <- subset(div_text, nchar(div_text) >= 20)

    registered <-
      which(grepl(pattern = "®|™", tolower(paragraphs)) == TRUE)

    regpatt <- "\\w*.\\w*®|\\w*.\\w*™"
    #dashpatt <- "(\\w+([-'\\p{Pd}])(\\w+)?(\\w+))®" # add this: (\\w+([-'])(\\w+)?[']?(\\w+))
    #testpatt<- "\\w.-\\s*®"
    # "\\w{4,}(®|™)|\\w*\\W\\w\\w?\\w?(®|™)"

    regwords1 <- list()
    checkthese <- data.frame()
    if (exists("engwords") == TRUE) rm(engwords)
    if (exists("registeredwords") == TRUE) rm(registeredwords)

    for (f in seq_along(paragraphs[registered])) {
      regwords1[f] <- str_extract_all(paragraphs[registered][f], "\\w{4,}(®|™)|\\w*\\W\\w\\w?\\w?(®|™)") # finds most words
       } # replaces entire list element with longer version

    regwords1 <- unlist(regwords1)

    if (is.null(regwords1) == FALSE) {
      output1 <- tibble::tibble(
      "Company" = company,
      "Month" = month,
      "Year" = year,
      "Words" = regwords1)

      o1 <- setDT(output1)
      write_csv(o1, path = paste0("~/git/business_innovation/data/business_innovation/working/sec/secregwordlists/",
                                str_extract(str_extract(i, "(?<=Edgar_filings_folders/)(.*)(?=.txt)"), "(?<=/)(.*)") , ".csv"))

      if (exists("fin_o") == FALSE)
        fin_o <- o1
      else
        fin_o <- rbindlist(list(fin_o, o1))

    }

  })
}

# Write to file

write_csv(fin_o, "data/business_innovation/working/sec/wordcounts_2000_2867.csv")



## Make english dictionary

# if(is.null(regwords1) == FALSE) {
#   checkthese <- regwords1 %>%
#   unlist() %>% unique() %>% str_trim %>%
#   str_split(" ") %>% unlist() %>% unique() %>% as.data.frame()
#   if(nrow(checkthese) > 0) {
#     colnames(checkthese) <- c("word")
#     checkthese$word <- as.character(checkthese$word)
#     engwords <- checkthese %>%  mutate(eng = hunspell_check(word)) %>% filter(eng == TRUE) %>% select(word)
#     if(nrow(engwords) > 0) {
#       engpatt <- paste(engwords$word, collapse = "|")
#       registeredwords <- regwords1 %>%
#         str_remove(pattern = engpatt) %>%
#         str_trim()





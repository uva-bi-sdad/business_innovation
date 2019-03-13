## LIBRARIES
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

## GRAB ALL PATHS
paths_file <- "data/business_innovation/original/edgar_filings/ALL_SEC_files.txt"
file_headers <- readr::read_tsv(paths_file, col_names = FALSE)
paths <- paste0("./data/business_innovation/original/edgar_filings/Edgar_filings_folders/", file_headers$X1)
paths[9]
file_names <- unique(list.files(paths, full.names = TRUE))
file_names[9]
head(file_names)

## CREATE FUNCTION
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

removeDocTypes <-
  function(xml_string, types = c("GRAPHIC", "EXCEL", "ZIP")) {
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
        no_ns <- paste0(substr(no_ns, 1, st), substr(no_ns, en, nchar(no_ns)))
      }
    }
    no_ns
  }



## for loop
if (exists("fin_o") == TRUE) rm(fin_o)

# Loop over file paths. For safety, I specify subsets (e.g. file_names[1:1000]) and
# then write each to a file, combining all files at the end
# use trycatch to skip errors
for (i in file_names[2002:2867]) {
  tryCatch({
    unclean <- read_file(i)
    #unclean=read_file(file_names[7])
    cleaned <- removeDocTypes(unclean)
    edgar <- read_html(cleaned)
    edgar <- edgar %>%
      as.character() %>%
      HTML() %>%
      read_html()
  }, warning = function(war) {
    print(paste("MY_WARNING:  ", war))
    return(NA)

  }, error = function(err) {
    print(paste("ERROR:  ", err))
    #o <- out.matrix[i, 1] <- paste(basename(repo), "ERROR")
    return(NA)

  }, finally = {
    metadata <- xml_find_all(edgar, ".//b")
    date <- html_text(metadata[7])
    #company <- html_text(metadata[14])
    company <- stringr::str_match(basename(i), "(^.*?)_")[, 2]
    date <-
      stringr::str_match(basename(i),
                         "([0-9][0-9][0-9][0-9])-([0-9][0-9])-[0-9][0-9]")
    month <- date[, 3]
    year <- date[, 2]
    #patt <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")
    #patt2 <-as.character(c(2010:2019))
    #month <- stringr::str_which(date, patt)
    #year <- patt2[stringr::str_which(date, patt2)]

    div <- xml_find_all(edgar, ".//p")
    div_text <- div %>%
      html_text() %>%
      str_squish

    paragraphs <- subset(div_text, nchar(div_text) >= 20)
    word_innov <- c("launch", "new product")

    innov_text <-
      which(grepl(paste(word_innov, collapse = "|"), tolower(paragraphs)) == TRUE)

    words <-
      unlist(stringr::str_split(gsub('[[:punct:] ]+', ' ', paragraphs[innov_text]), " ", simplify = FALSE))
    words <- words %>%
      str_replace(pattern = "\u0092", "") %>%
      str_replace(pattern = "\u0093", "") %>%
      str_replace(pattern = "\u0094", "") %>%
      str_replace(pattern = "\u0095", "") %>%
      str_replace(pattern = "\u0097", "") %>%
      str_replace(pattern = "\u0099", "")
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
    # output2 <- output1 %>%
    #   filter(English == FALSE & Capitals == TRUE & nchar(Words)>3 & substrRight(Words, 1) != "s") %>%
    #   group_by(Words) %>%
    #   summarise(count = n()) %>%
    #   arrange(desc(count)) %>%
    #   data.table::as.data.table()

    #print(output1)

    # Group by Company and Words and get count of records for each group. Then order by count descending.
    o1 <- setDT(output1)
    o2 <-
      o1[English == FALSE & Capitals == TRUE & nchar(Words) > 3 & substrRight(Words, 1) != "s",
         .(count = .N),
         .(Company, Words)][order(-count)]

    print(o2)

    # combine results with final data.table
    if (exists("fin_o") == FALSE)
      fin_o <- o2
    else
      fin_o <- rbindlist(list(fin_o, o2))
  })
}

# Group output by Company and Words, then sum the counts
fin_o <- fin_o[, .(tot_cnt=sum(count)),.(Company, Words)][order(Company, -tot_cnt)]

# Write to file
write_csv(fin_o, "data/business_innovation/working/word_counts_2001_2867.csv")

# Combine files
f1 <- fread("data/business_innovation/working/word_counts_1_1000.csv")
f2 <- fread("data/business_innovation/working/word_counts_1001_2000.csv")
f3 <- fread("data/business_innovation/working/word_counts_2001_2867.csv")
f_all <- rbindlist(list(f1,f2,f3))
# Group by Company and Words, then sum all tot_cnts
f_all <- f_all[, .(tot_cnt=sum(tot_cnt)),.(Company, Words)][order(Company, -tot_cnt)]
# Write combined to a file
write_csv(f_all, "data/business_innovation/working/word_counts_all.csv")



# Add company names to combined file
f_all_2 <- fread("data/business_innovation/working/word_counts_all.csv")
master_index <- readRDS("~/git/business_innovation/data/business_innovation/original/master_index.RDS")
# Get unique CIK and Company Names
cik_unique <- unique(master_index[, .(CIK, COMPANY_NAME)])
# Merge with combined file
f_all_names <- merge(f_all_2, cik_unique, by = "CIK", all.x = TRUE)

# Group by CIK, Words, and tot_cnt, then collapse multiple names for a CIK into a comma delimited list
f_all_names_cat <- f_all_names[, .(company_name = paste(COMPANY_NAME, collapse = ", ")), .(CIK, Words, tot_cnt)]
# Write combined file with names to a file
write_csv(f_all_names_cat, "data/business_innovation/working/word_counts_w_cat_names_all.csv")



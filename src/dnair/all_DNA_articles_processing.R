## LIBRARIES

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
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
datapath <- "/project/biocomplexity/sdad/projects-active/volume_nyc1_01/business_innovation/" #new rivanna location
datapath_perm <- "/project/biocomplexity/sdad/projects-active/ncses/bi/" #new NEW rivanna location
dna_2015 <- readRDS(paste0(datapath, "working/DNA_Aggregated/dna_2015.RDS"))
dna_cmda_hlbody <- readr::read_csv(paste0(datapath, "working/DNA_Aggregated/CMDA_FALL2019/CMDA_FALL2019_hlwbody_hlbasedlabel.csv"))

dna_cmda_hlbody <- dna_cmda_hlbody %>% transmute(id, label = recode(innovYN, `No` = 0, `no` = 0, `Yes` = 1, `yes` = 1), hl = title, text = paste0(title, body))
#saveRDS(dna_cmda_hlbody, file = paste0("data/dna_cmda_clean.RDS")) # Needs to move to new NEW rivanna location

# colnames(dna_2015)
# dna_2015_hl = dna_2015 %>% select(an, title)
# dna_2015_hl = dna_2015 %>% select(an, body)
#dna_2015_body = dna_2015 %>% select(an, body)
# test <- head(dna_2015_hl)
# dna_2015_hl

# test2 <- list.files("~/git/business_innovation/data/working/secmethod_DNAdata/dna_15_hl_eng_wordlists/")
# length(test2) #84995

## remove final data.table if exists
if (exists("fin_o") == TRUE) rm(fin_o)

# Loop over file paths. For safety, I specify subsets (e.g. file_names[1:1000]) and
# then write each to a file, combining all files at the end
# use trycatch to skip errors
for (i in 84995:nrow(dna_2015_hl)) {
    article_num <- dna_2015_hl$an[i]
    char_ct <- nchar(dna_2015_hl$title[i])
    words <-
      unlist(stringr::str_split(gsub('[[:punct:] ]+', ' ', dna_2015_hl$title[i]), " ", simplify = FALSE))
    regwords <- unlist(str_extract_all(dna_2015_hl$title[i], "\\w{4,}(®|™)|\\w*\\W\\w\\w?\\w?(®|™)"))
    eng <- hunspell_check(words, dict = dictionary("en_US"))
    caps <- grepl("^[[:upper:]]", words)

    output1 <- tibble::tibble(
      "art_num" = article_num,
      "char_ct" = char_ct,
      "Words" = words,
      "English" = eng,
      "Capitals" = caps
    ) %>% filter(!dataplumbr::var.is_blank(Words))

    # # output2 <- output1 %>%
    # #   filter(English == FALSE & Capitals == TRUE & nchar(Words)>3 & substrRight(Words, 1) != "s") %>%
    # #   group_by(Words) %>%
    # #   summarise(count = n()) %>%
    # #   arrange(desc(count)) %>%
    # #   data.table::as.data.table()
    # output1

    # Group by Company and Words and get count of records for each group. Then order by count descending.
    o1 <- setDT(output1)
    o2 <-
      o1[English == FALSE & Capitals == TRUE & nchar(Words) > 3 & substrRight(Words, 1) != "s",
         .(count = .N),
         .(art_num, char_ct, Words)][order(-count)]
    write_csv(o1, path = paste0("~/git/business_innovation/data/working/secmethod_DNAdata/dna_15_hl_eng_wordlists/", article_num , ".csv"))

    #print(o2)

    # combine results with final data.table
    if (exists("fin_o") == FALSE)
      fin_o <- o2
    else
      fin_o <- rbindlist(list(fin_o, o2))
  }

# Write to file

#write_csv(fin_o, "~/git/business_innovation/data/working/secmethod_DNAdata/dna_15_hl_eng_wordlists_1_84995.csv")


#filter(fin_o, Words == "Neutrolin")
# reshape2::dcast(fin_o, Company + Words ~ Year, sum)
# reshape2::dcast(fin_o, Words + Company ~ Year, sum)
#
# fin_o$Company <- as.numeric(fin_o$Company)
#
# fin_o1 <- fin_o %>%
#   left_join(cik_ticker, by = c("Company" = "CIK"))
#
# filter(fin_o1, Incorporated == "L2")
# unique(fin_o1$Incorporated)

# tidyr::gather(data = fin_o, key = "Company", value = "Year", count)
# tidyr::gather(data = fin_o, key = Company, value = Year)
# test <- tidyr::gather(data = fin_o, key = key, value = value)
# unique(test$key)

# # Group output by Company and Words, then sum the counts
# fin_o <- fin_o[, .(tot_cnt=sum(count)),.(Company, Words)][order(Company, -tot_cnt)]
#

#
#
# # Combine files
# f1 <- fread("data/business_innovation/working/dnword_counts_1_1000.csv")
# f2 <- fread("data/business_innovation/working/dnword_counts_1001_2000.csv")
# f3 <- fread("data/business_innovation/working/dnword_counts_2001_2867.csv")
# f_all <- rbindlist(list(f1,f2,f3))
# # Group by Company and Words, then sum all tot_cnts
# f_all <- f_all[, .(tot_cnt=sum(tot_cnt)),.(Company, Words)][order(Company, -tot_cnt)]
# # Write combined to a file
# write_csv(f_all, "data/business_innovation/working/dnword_counts_all.csv")
# #----
#
#
# # Add company names to combined file
# f_all_2 <- fread("data/business_innovation/working/word_counts_all.csv")
# master_index <- readRDS("~/git/business_innovation/data/business_innovation/original/master_index.RDS")
# # Get unique CIK and Company Names
# cik_unique <- unique(master_index[, .(CIK, COMPANY_NAME)])
# # Merge with combined file
# f_all_names <- merge(f_all_2, cik_unique, by = "CIK", all.x = TRUE)
# # Group by CIK, Words, and tot_cnt, then collapse multiple names for a CIK into a comma delimited list
# f_all_names_cat <- f_all_names[, .(company_name = paste(COMPANY_NAME, collapse = ", ")), .(CIK, Words, tot_cnt)]
# # Write combined file with names to a file
# write_csv(f_all_names_cat, "data/business_innovation/working/word_counts_w_cat_names_all.csv")



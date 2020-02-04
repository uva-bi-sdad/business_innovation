## LIBRARIES

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}


library(stringr)
library(hunspell)
library(data.table)
library(dplyr)
library(magrittr)
library(readr)

##GRAB METADATA
#dna_2015 <- readRDS("~/git/business_innovation/data/working/DNA_Aggregated/dna_2015.RDS")
# colnames(dna_2015)
#dna_2015_hl = dna_2015 %>% select(an, title)
#dna_2015_body = dna_2015 %>% select(an, body)
dna_2015_hl <- readRDS("~/git/business_innovation/data/working/secmethod_DNAdata/dna_2015_hl.RDS")
# test <- head(dna_2015_hl)
# dna_2015_hl

# test2 <- list.files("~/git/business_innovation/data/working/secmethod_DNAdata/dna_15_hl_eng_wordlists/")
# length(test2) #84995

## remove final data.table if exists
if (exists("fin_o") == TRUE) rm(fin_o)

# Loop over file paths. For safety, I specify subsets (e.g. file_names[1:1000]) and
# then write each to a file, combining all files at the end
# use trycatch to skip errors
for (i in 777:nrow(dna_2015_hl)) {
  print(i)
  article_num <- dna_2015_hl$an[i]
  char_ct <- nchar(dna_2015_hl$title[i])
  regwords <- unlist(str_extract_all(dna_2015_hl$title[i], "\\w{4,}(®|™)|\\w*\\W\\w\\w?\\w?(®|™)"))

  output1 <- tibble::tibble(
    "art_num" = article_num,
    "char_ct" = char_ct,
    "Words" = regwords
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
    o1[nchar(Words) > 3,
       .(count = .N),
       .(art_num, char_ct, Words)][order(-count)]

  if (nrow(o1) > 0)
    write_csv(o1, path = paste0("~/git/business_innovation/data/working/secmethod_DNAdata/dna_15_hl_reg_wordlists/", article_num , ".csv"))

  # combine results with final data.table
  if (exists("fin_o") == FALSE)
    fin_o <- o2
  else
    fin_o <- rbindlist(list(fin_o, o2))
}

# Write to file

#write_csv(fin_o, "~/git/business_innovation/data/working/secmethod_DNAdata/dna_15_hl_reg_wordlists_all.csv")


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



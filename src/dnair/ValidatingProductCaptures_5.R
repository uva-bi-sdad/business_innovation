library(dplyr)
library(stringr)
library(ggplot2)

datapath <- "/project/biocomplexity/sdad/projects_data/volume_nyc1_01/business_innovation/" #new rivanna location
datapath1 <- "/project/biocomplexity/sdad/projects_data/ncses/bi/binn/"
datapath2 <- "/project/biocomplexity/sdad/projects_data/ncses/bi/binn/" #new rivanna location
ndc_product <-  readr::read_tsv(paste0(datapath, "original/NDC/product.txt")) %>% mutate(year = str_extract(string = STARTMARKETINGDATE, pattern = "^\\d{4}"))
final_product_list <- readRDS(paste0(datapath, "working/sec/finalwordlists/final_product_list_dec20.RDS"))
final_product_list_singlefirstmentionsONLY <- readRDS(paste0(datapath, "working/sec/finalwordlists/final_product_list_singmention_dec20.RDS"))

length(unique(ndc_product$year))

#ndc_sec_ref <- readRDS(paste0(datapath1, "working/NDC/ndc_sec_ref90.RDS"))
ndc_sec_ref <- readxl::read_excel(paste0(datapath2, "working/NDC_SEC_CompNames/final_ndc_sec_companies.xlsx"), sheet = "finalset") %>% select(-12,-13, -14)
colnames(ndc_sec_ref) <- dataplumbr::name.standard_col_names(colnames(ndc_sec_ref))

#ndc_sec_ref %>% filter(lh_matches > 1) %>% View()
nrow(ndc_sec_ref)
length(unique(ndc_sec_ref$family))
length(unique(ndc_sec_ref$cik))
length(unique(ndc_sec_ref$secname))
length(unique(ndc_sec_ref$sec))
length(unique(ndc_sec_ref$ndc_labeler))
lnums <- unique(str_trim(unlist(str_split(ndc_sec_ref$lnum, ","))))
length(lnums[!is.na(lnums)])

ndc_percorpfamily_ct <- ndc_sec_ref %>% count(family, ndc_labeler) %>% select(-n) %>% count(family)
sec_percorpfamily_ct <- ndc_sec_ref %>% count(family, cik) %>% select(-n) %>% count(family)
max(ndc_percorpfamily_ct$n)

table(sec_percorpfamily_ct$n)
table(ndc_percorpfamily_ct$n)

dc_sec_ref %>% count(method)

ndc_sec_ref %>% count(family, cik, ndc_labeler) %>% select(-n) %>% count(family) %>% filter(n == 1) %>% View()
ndc_sec_ref %>% count(family, cik, ndc_labeler) %>% select(-n) %>% count(family) %>% filter(n > 1) %>% View()


ggplot(ndc_percorpfamily_ct, aes(x = reorder(family, X = n), y = n)) +
  geom_bar(stat = "identity") +
  coord_flip()

ggplot(sec_percorpfamily_ct, aes(x = reorder(family, X = n), y = n)) +
  geom_bar(stat = "identity") +
  coord_flip()

secmults <- sec_percorpfamily_ct %>% filter(n > 1) %>% .$family
ndcmults <- ndc_percorpfamily_ct %>% filter(n > 1) %>% .$family

ndc_sec_ref %>% select(1,4,5) %>% distinct() %>% filter(family %in% secmults)
ndc_sec_ref %>% select(1,10) %>% distinct() %>% filter(family %in% ndcmults) %>% View()

ndc_sec_ref

#### slide 31

ndc_validateset <- ndc_product %>%
  select(21, 4, 6, 13, 14) %>%
  distinct() %>%
  filter(LABELERNAME %in% ndc_sec_ref$ndc_labeler) %>%
  mutate(Prop_low = str_trim(str_replace_all(str_remove_all(str_to_lower(PROPRIETARYNAME), "\\(|\\)"), "\\\u0097|\\\u0093", " ")),  #str_to_lower(PROPRIETARYNAME),
         Nonprop_low = str_trim(str_replace_all(str_remove_all(str_to_lower(NONPROPRIETARYNAME), "\\(|\\)"), "\\\u0097|\\\u0093", " ")), # str_to_lower(NONPROPRIETARYNAME),
         Sub_low = str_trim(str_replace_all(str_remove_all(str_to_lower(SUBSTANCENAME), "\\(|\\)"), "\\\u0097|\\\u0093", " ")))

ndc_validateset %>% count(year, LABELERNAME, PROPRIETARYNAME, NONPROPRIETARYNAME, SUBSTANCENAME) %>% nrow()

nrow(final_product_list) - nrow(final_product_list_singlefirstmentionsONLY)
nrow(ndc_product) - nrow(ndc_validateset)

length(unique(ndc_validateset$PROPRIETARYNAME))
length(unique(ndc_validateset$Prop_low))
length(unique(ndc_validateset$NONPROPRIETARYNAME))
length(unique(ndc_validateset$Nonprop_low))
length(unique(ndc_validateset$SUBSTANCENAME))
length(unique(ndc_validateset$Sub_low))
length(unique(ndc_validateset$PROPRIETARYNAME)) - length(unique(ndc_validateset$Prop_low))
length(unique(ndc_validateset$NONPROPRIETARYNAME)) - length(unique(ndc_validateset$Nonprop_low))
length(unique(ndc_validateset$SUBSTANCENAME)) - length(unique(ndc_validateset$Sub_low))

nrow(final_product_list_singlefirstmentionsONLY)
final_product_list_singlefirstmentionsONLY %>% count(Token, Protect) %>% select(-n) %>% nrow()
length(unique(final_product_list_singlefirstmentionsONLY$Token))


##did this one for marketing categories
ndc_validateset_markcat <- ndc_product %>%
  select(21, 4, 6, 13, 14, 3, 11) %>%
  distinct() %>%
  filter(LABELERNAME %in% ndc_sec_ref$NDCCompany) %>%
  mutate(Prop_low = str_trim(str_replace_all(str_remove_all(str_to_lower(PROPRIETARYNAME), "\\(|\\)"), "\\\u0097|\\\u0093", " ")),  #str_to_lower(PROPRIETARYNAME),
         Nonprop_low = str_trim(str_replace_all(str_remove_all(str_to_lower(NONPROPRIETARYNAME), "\\(|\\)"), "\\\u0097|\\\u0093", " ")), # str_to_lower(NONPROPRIETARYNAME),
         Sub_low = str_trim(str_replace_all(str_remove_all(str_to_lower(SUBSTANCENAME), "\\(|\\)"), "\\\u0097|\\\u0093", " "))) # str_to_lower(SUBSTANCENAME))

captures <- final_product_list_singlefirstmentionsONLY %>% select(first_mention, Token, Name, Protect )
#nrow(captures) - 4651

captures <- captures %>% mutate(capture = str_trim(str_replace_all(str_remove_all(str_to_lower(Token), "\\(|\\)"), "\\\u0097|\\\u0093", " ")))
#length(unique(captures$capture)) # 4156
#captures %>% count(capture) %>% filter(n > 1)
captures_patt <- paste0("\\b", "(", paste0(paste(unique(captures$capture), sep = ""), collapse = "|"), ")", "\\b")
#length(str_trim(str_replace_all(str_remove_all(str_to_lower(unique(captures$Token)), "\\(|\\)"), "\\\u0097|\\\u0093", " ")))


# captures_122 <- final_product_list_singlefirstmentionsONLY %>% select(first_mention, Token, Name, Protect ) %>% filter(Name %in% ndc_sec_ref$SECCompany)
# #nrow(captures) - 4651
#
# captures_122 <- captures_122 %>% mutate(capture = str_trim(str_replace_all(str_remove_all(str_to_lower(Token), "\\(|\\)"), "\\\u0097|\\\u0093", " ")))
# #length(unique(captures$capture)) # 4156
# #captures %>% count(capture) %>% filter(n > 1)
# captures_patt <- paste0("\\b", "(", paste0(paste(unique(captures_122$capture), sep = ""), collapse = "|"), ")", "\\b")
#length(str_trim(str_replace_all(str_remove_all(str_to_lower(unique(captures$Token)), "\\(|\\)"), "\\\u0097|\\\u0093", " ")))

library(data.table)
library(maditr)

### ALL TIME
prop_results_mar2 <- ndc_validateset %>% as.data.table() %>%
  dt_select(year, PROPRIETARYNAME, Prop_low) %>%
  dt_mutate(prop_match = str_extract_all(Prop_low, pattern = captures_patt),
            matches = lengths(prop_match))

table(prop_results_mar2$matches)
prop_results_mar2 %>% filter(matches == 0)

prop_pos_matches_mar2 <- prop_results_mar2 %>% tidyr::unnest(cols = c(prop_match))
prop_pos_matches_sec_ndc_mar2 <- prop_pos_matches_mar2 %>%
  left_join(captures, by = c("prop_match" = "capture")) %>%
  left_join(final_product_list_singlefirstmentionsONLY, by = c("first_mention", "Token", "Name", "Protect")) %>%
  left_join(ndc_validateset %>% select(year, PROPRIETARYNAME, LABELERNAME, Prop_low), by = c("PROPRIETARYNAME", "Prop_low", "year"))


### DID THIS TO MAKE MARKETING CATEGORIES WORK for gary

#used the read out prop_results
prop_pos_matches_2 <- prop_results_mar2 %>% tidyr::unnest(cols = c(prop_match))
prop_pos_matches_sec_ndc_2 <- prop_pos_matches_2 %>%
  left_join(captures, by = c("prop_match" = "capture")) %>%
  left_join(final_product_list_singlefirstmentionsONLY, by = c("first_mention", "Token", "Name", "Protect")) %>%
  left_join(ndc_validateset_markcat %>% select(year, PROPRIETARYNAME, LABELERNAME, PRODUCTTYPENAME, MARKETINGCATEGORYNAME,  Prop_low), by = c("PROPRIETARYNAME", "Prop_low", "year"))

# saveRDS(prop_results_mar2, paste0(datapath1, "working/NDC/ndc_prop_match_results_mar2.RDS")) # "git/business_innovation/data/ndc_prop_match_results_122.RDS")
# saveRDS(prop_pos_matches_sec_ndc_mar2, paste0(datapath1, "working/NDC/ndc_sec_prop_positive_match_results_mar2.RDS"))
#saveRDS(prop_pos_matches_sec_ndc_2, paste0(datapath1, "working/NDC/ndc_sec_prop_posmatch_markcatgary_results_mar2.RDS"))

nonprop_results_mar2 <- ndc_validateset %>%  as.data.table() %>%
  dt_select(year, NONPROPRIETARYNAME, Nonprop_low) %>%
  dt_mutate(nonprop_match = str_extract_all(Nonprop_low, pattern = captures_patt),
            matches = lengths(nonprop_match))

table(nonprop_results_mar2$matches)
nonprop_results_mar2 %>% filter(matches == 0)

nonprop_pos_matches_mar2 <- nonprop_results_mar2 %>% tidyr::unnest(cols = c(nonprop_match))
nonprop_pos_matches_sec_ndc_mar2 <- nonprop_pos_matches_mar2 %>%
  left_join(captures, by = c("nonprop_match" = "capture")) %>%
  left_join(final_product_list_singlefirstmentionsONLY, by = c("first_mention", "Token", "Name", "Protect")) %>%
  left_join(ndc_validateset %>% select(year, NONPROPRIETARYNAME, LABELERNAME, Nonprop_low), by = c("NONPROPRIETARYNAME", "Nonprop_low", "year"))

### DID THIS TO MAKE MARKETING CATEGORIES WORK for gary

#used the read out prop_results
nonprop_pos_matches_2 <- nonprop_results_mar2 %>% tidyr::unnest(cols = c(nonprop_match))
nonprop_pos_matches_sec_ndc_2 <- nonprop_pos_matches_2 %>%
  left_join(captures, by = c("nonprop_match" = "capture")) %>%
  left_join(final_product_list_singlefirstmentionsONLY, by = c("first_mention", "Token", "Name", "Protect")) %>%
  left_join(ndc_validateset_markcat %>% select(year, NONPROPRIETARYNAME, LABELERNAME, PRODUCTTYPENAME, MARKETINGCATEGORYNAME,  Nonprop_low), by = c("NONPROPRIETARYNAME", "Nonprop_low", "year"))

# saveRDS(nonprop_pos_matches_sec_ndc_2, paste0(datapath1, "working/NDC/ndc_sec_nonprop_posmatch_markcatgary_resultsmar2.RDS"))
# saveRDS(nonprop_results_mar2, paste0(datapath1, "working/NDC/ndc_nonprop_match_resultsmar2.RDS"))  # "git/business_innovation/data/ndc_prop_match_results_122.RDS")
# saveRDS(nonprop_pos_matches_sec_ndc_mar2, paste0(datapath1, "working/NDC/ndc_sec_nonprop_positive_match_resultsmar2.RDS"))


sub_results_mar2 <- ndc_validateset %>% as.data.table() %>%
  dt_select(year, SUBSTANCENAME, Sub_low) %>%
  filter(!is.na(SUBSTANCENAME)) %>%
  dt_mutate(sub_match = str_extract_all(Sub_low, pattern = captures_patt),
            matches = lengths(sub_match))

table(sub_results_mar2$matches)
sub_results_mar2 %>% filter(matches == 0)

sub_pos_matches_mar2 <- sub_results_mar2 %>% tidyr::unnest(cols = c(sub_match))
sub_pos_matches_sec_ndc_mar2 <- sub_pos_matches_mar2 %>%
  left_join(captures, by = c("sub_match" = "capture")) %>%
  left_join(final_product_list_singlefirstmentionsONLY, by = c("first_mention", "Token", "Name", "Protect")) %>%
  left_join(ndc_validateset %>% select(year, SUBSTANCENAME, LABELERNAME, Sub_low), by = c("SUBSTANCENAME", "Sub_low", "year"))

# saveRDS(sub_results_mar2, paste0(datapath1, "working/NDC/ndc_sub_match_resultsmar2.RDS"))# "git/business_innovation/data/ndc_prop_match_results_122.RDS")
# saveRDS(sub_pos_matches_sec_ndc_mar2, paste0(datapath1, "working/NDC/ndc_sec_sub_positive_match_resultsmar2.RDS"))

## TRYING ALL NAMES

allname_results_mar2 <- ndc_validateset %>% as.data.table() %>%
  dt_mutate(prop_match = str_extract_all(Prop_low, pattern = captures_patt),
            prop_matches = lengths(prop_match),
            nonprop_match = str_extract_all(Nonprop_low, pattern = captures_patt),
            nonprop_matches = lengths(nonprop_match),
            sub_match = str_extract_all(Sub_low, pattern = captures_patt),
            sub_matches = lengths(sub_match))

table(allname_results_mar2$prop_matches)
table(allname_results_mar2$nonprop_matches)
table(allname_results_mar2$sub_matches)

allname_results_mar2 <- allname_results_mar2 %>%dt_mutate(match_at_all = ifelse(prop_matches + nonprop_matches + sub_matches > 0, 1, 0))
table(allname_results_mar2$match_at_all)

allname_results_mar2_summary  <- allname_results_mar2 %>% dt_select(year, LABELERNAME, PROPRIETARYNAME, NONPROPRIETARYNAME, SUBSTANCENAME, match_at_all, prop_matches, nonprop_matches, sub_matches)

#saveRDS(allname_results_mar2, paste0(datapath1, "working/NDC/ndc_all_match_resultsmar2.RDS"))
#saveRDS(allname_results_mar2, paste0(datapath1, "working/NDC/ndc_all_match_resultssummarymar2.RDS"))

ggplot(data = allname_results_mar2_summary, aes(x = year, fill = match_at_all)) +
  geom_bar(stat = "count")



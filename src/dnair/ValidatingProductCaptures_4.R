library(dplyr)
library(stringr)

datapath <- "/project/biocomplexity/sdad/projects-active/volume_nyc1_01/business_innovation/" #new rivanna location
ndc_product <-  readr::read_tsv(paste0(datapath, "original/NDC/product.txt"))
final_product_list <- readRDS(paste0(datapath, "working/sec/finalwordlists/final_product_list_dec20.RDS"))
final_product_list_singlefirstmentionsONLY <- readRDS(paste0(datapath, "working/sec/finalwordlists/final_product_list_singmention_dec20.RDS"))
ndc_sec_ref <- readRDS(paste0(datapath1, "working/NDC/ndc_sec_ref90.RDS"))
ndc_sec_ref <- ndc_sec_ref %>% select(-l_num) %>% distinct()

ndc_names <- ndc_product %>%
  mutate(year = str_extract(string = STARTMARKETINGDATE, pattern = "^\\d{4}")) %>%
  select(21, 4, 6, 13, 14) %>%
  distinct() %>%
  mutate(Prop_low = str_trim(str_replace_all(str_remove_all(str_to_lower(PROPRIETARYNAME), "\\(|\\)"), "\\\u0097|\\\u0093", " ")),  #str_to_lower(PROPRIETARYNAME),
         Nonprop_low = str_trim(str_replace_all(str_remove_all(str_to_lower(NONPROPRIETARYNAME), "\\(|\\)"), "\\\u0097|\\\u0093", " ")), # str_to_lower(NONPROPRIETARYNAME),
         Sub_low = str_trim(str_replace_all(str_remove_all(str_to_lower(SUBSTANCENAME), "\\(|\\)"), "\\\u0097|\\\u0093", " "))) # str_to_lower(SUBSTANCENAME))

ndc_names_122 <- ndc_product %>%
  mutate(year = str_extract(string = STARTMARKETINGDATE, pattern = "^\\d{4}")) %>%
  select(21, 4, 6, 13, 14) %>%
  distinct() %>%
  filter(LABELERNAME %in% ndc_sec_ref$NDCCompany) %>%
  mutate(Prop_low = str_trim(str_replace_all(str_remove_all(str_to_lower(PROPRIETARYNAME), "\\(|\\)"), "\\\u0097|\\\u0093", " ")),  #str_to_lower(PROPRIETARYNAME),
         Nonprop_low = str_trim(str_replace_all(str_remove_all(str_to_lower(NONPROPRIETARYNAME), "\\(|\\)"), "\\\u0097|\\\u0093", " ")), # str_to_lower(NONPROPRIETARYNAME),
         Sub_low = str_trim(str_replace_all(str_remove_all(str_to_lower(SUBSTANCENAME), "\\(|\\)"), "\\\u0097|\\\u0093", " "))) # str_to_lower(SUBSTANCENAME))
##did this one for marketing categories
ndc_names_2 <- ndc_product %>%
  mutate(year = str_extract(string = STARTMARKETINGDATE, pattern = "^\\d{4}")) %>%
  select(21, 4, 6, 13, 14, 3, 11) %>%
  distinct() %>%
  filter(LABELERNAME %in% ndc_sec_ref$NDCCompany) %>%
  mutate(Prop_low = str_trim(str_replace_all(str_remove_all(str_to_lower(PROPRIETARYNAME), "\\(|\\)"), "\\\u0097|\\\u0093", " ")),  #str_to_lower(PROPRIETARYNAME),
         Nonprop_low = str_trim(str_replace_all(str_remove_all(str_to_lower(NONPROPRIETARYNAME), "\\(|\\)"), "\\\u0097|\\\u0093", " ")), # str_to_lower(NONPROPRIETARYNAME),
         Sub_low = str_trim(str_replace_all(str_remove_all(str_to_lower(SUBSTANCENAME), "\\(|\\)"), "\\\u0097|\\\u0093", " "))) # str_to_lower(SUBSTANCENAME))

nrow(ndc_names_122)
length(unique(ndc_names_122$PROPRIETARYNAME))

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
prop_results_122 <- ndc_names_122 %>% as.data.table() %>%
  dt_select(year, PROPRIETARYNAME, Prop_low) %>%
  dt_mutate(prop_match = str_extract_all(Prop_low, pattern = captures_patt),
            matches = lengths(prop_match))

table(prop_results_122$matches)
prop_results_122 %>% filter(matches == 0)

prop_pos_matches_122 <- prop_results_122 %>% tidyr::unnest(cols = c(prop_match))
prop_pos_matches_sec_ndc_122 <- prop_pos_matches_122 %>%
  left_join(captures, by = c("prop_match" = "capture")) %>%
  left_join(final_product_list_singlefirstmentionsONLY, by = c("first_mention", "Token", "Name", "Protect")) %>%
  left_join(ndc_names %>% select(year, PROPRIETARYNAME, LABELERNAME, Prop_low), by = c("PROPRIETARYNAME", "Prop_low", "year"))


### DID THIS TO MAKE MARKETING CATEGORIES WORK for gary

#used the read out prop_results
prop_pos_matches_2 <- prop_results %>% tidyr::unnest(cols = c(prop_match))
prop_pos_matches_sec_ndc_2 <- prop_pos_matches_2 %>%
  left_join(captures, by = c("prop_match" = "capture")) %>%
  left_join(final_product_list_singlefirstmentionsONLY, by = c("first_mention", "Token", "Name", "Protect")) %>%
  left_join(ndc_names_2 %>% select(year, PROPRIETARYNAME, LABELERNAME, PRODUCTTYPENAME, MARKETINGCATEGORYNAME,  Prop_low), by = c("PROPRIETARYNAME", "Prop_low", "year"))


#saveRDS(prop_pos_matches_sec_ndc_2, paste0(datapath1, "working/NDC/ndc_sec_prop_posmatch_markcatgary_results.RDS"))

# saveRDS(prop_results_122, paste0(datapath1, "working/NDC/ndc_prop_match_results.RDS")) # "git/business_innovation/data/ndc_prop_match_results_122.RDS")
#saveRDS(prop_pos_matches_sec_ndc_122, paste0(datapath1, "working/NDC/ndc_sec_prop_positive_match_results.RDS"))

nonprop_results_122 <- ndc_names_122 %>%  as.data.table() %>%
  dt_select(year, NONPROPRIETARYNAME, Nonprop_low) %>%
  dt_mutate(nonprop_match = str_extract_all(Nonprop_low, pattern = captures_patt),
            matches = lengths(nonprop_match))

table(nonprop_results_122$matches)
nonprop_results_122 %>% filter(matches == 0)

nonprop_pos_matches_122 <- nonprop_results_122 %>% tidyr::unnest(cols = c(nonprop_match))
nonprop_pos_matches_sec_ndc_122 <- nonprop_pos_matches_122 %>%
  left_join(captures, by = c("nonprop_match" = "capture")) %>%
  left_join(final_product_list_singlefirstmentionsONLY, by = c("first_mention", "Token", "Name", "Protect")) %>%
  left_join(ndc_names %>% select(year, NONPROPRIETARYNAME, LABELERNAME, Nonprop_low), by = c("NONPROPRIETARYNAME", "Nonprop_low", "year"))

### DID THIS TO MAKE MARKETING CATEGORIES WORK for gary

#used the read out prop_results
nonprop_pos_matches_2 <- nonprop_results %>% tidyr::unnest(cols = c(nonprop_match))
nonprop_pos_matches_sec_ndc_2 <- nonprop_pos_matches_2 %>%
  left_join(captures, by = c("nonprop_match" = "capture")) %>%
  left_join(final_product_list_singlefirstmentionsONLY, by = c("first_mention", "Token", "Name", "Protect")) %>%
  left_join(ndc_names_2 %>% select(year, NONPROPRIETARYNAME, LABELERNAME, PRODUCTTYPENAME, MARKETINGCATEGORYNAME,  Nonprop_low), by = c("NONPROPRIETARYNAME", "Nonprop_low", "year"))

#saveRDS(nonprop_pos_matches_sec_ndc_2, paste0(datapath1, "working/NDC/ndc_sec_nonprop_posmatch_markcatgary_results.RDS"))
#saveRDS(nonprop_results_122, paste0(datapath1, "working/NDC/ndc_nonprop_match_results.RDS"))
# "git/business_innovation/data/ndc_prop_match_results_122.RDS")
#saveRDS(nonprop_pos_matches_sec_ndc_122, paste0(datapath1, "working/NDC/ndc_sec_nonprop_positive_match_results.RDS"))


sub_results_122 <- ndc_names_122 %>% as.data.table() %>%
  dt_select(year, SUBSTANCENAME, Sub_low) %>%
  filter(!is.na(SUBSTANCENAME)) %>%
  dt_mutate(sub_match = str_extract_all(Sub_low, pattern = captures_patt),
            matches = lengths(sub_match))

table(sub_results_122$matches)
sub_results_122 %>% filter(matches == 0)

sub_pos_matches_122 <- sub_results_122 %>% tidyr::unnest(cols = c(sub_match))
sub_pos_matches_sec_ndc_122 <- sub_pos_matches_122 %>%
  left_join(captures, by = c("sub_match" = "capture")) %>%
  left_join(final_product_list_singlefirstmentionsONLY, by = c("first_mention", "Token", "Name", "Protect")) %>%
  left_join(ndc_names %>% select(year, SUBSTANCENAME, LABELERNAME, Sub_low), by = c("SUBSTANCENAME", "Sub_low", "year"))

#saveRDS(sub_results_122, paste0(datapath1, "working/NDC/ndc_sub_match_results.RDS"))
# "git/business_innovation/data/ndc_prop_match_results_122.RDS")
#saveRDS(sub_pos_matches_sec_ndc_122, paste0(datapath1, "working/NDC/ndc_sec_sub_positive_match_results.RDS"))



### Year specific


ndc <- ndc_names %>% filter(year > 2011 & year < 2018)
ndc_by_yr <- split(x = ndc, f = ndc$year )
captures_by_yr <- split(captures, f = captures$first_mention)

library(data.table)
library(maditr)

patt <- list()

for (i in 1:length(captures_by_yr)) {
  capts <- unique(captures_by_yr[[i]]$Token)
  patt[i] <- paste0("\\b", "(", paste0( paste(str_trim(str_replace_all(str_remove_all(str_to_lower(capts), "\\(|\\)"), "\\\u0097|\\\u0093", " ")), sep = ""), collapse = "|"), ")", "\\b")
  patt <- unlist(patt)
}

ndc_p_res_by_yr <- list()
ndc_np_res_by_yr <- list()
ndc_s_res_by_yr <- list()

for (i in 1:length(ndc_by_yr)) {
  ndc_p_res_by_yr[[i]] <- ndc_by_yr[[i]] %>% as.data.table() %>%
    dt_select(year, PROPRIETARYNAME, Prop_low) %>%
    dt_mutate(prop_match = str_extract_all(Prop_low, pattern = patt[i]),
              matches = lengths(prop_match))

  ndc_np_res_by_yr[[i]] <- ndc_by_yr[[i]] %>% as.data.table() %>%
    dt_select(year, PROPRIETARYNAME, Prop_low) %>%
    dt_mutate(prop_match = str_extract_all(Prop_low, pattern = patt[i]),
              matches = lengths(prop_match))

  ndc_s_res_by_yr[[i]] <- ndc_by_yr[[i]] %>% as.data.table() %>%
    dt_select(year, PROPRIETARYNAME, Prop_low) %>%
    dt_mutate(prop_match = str_extract_all(Prop_low, pattern = patt[i]),
              matches = lengths(prop_match))
}

saveRDS(ndc_p_res_by_yr, "~/git/business_innovation/ yearly_prop_match_results.RDS")
saveRDS(ndc_np_res_by_yr, "~/git/business_innovation/data/yearly_nonprop_match_results.RDS")
saveRDS(ndc_s_res_by_yr, "~/git/business_innovation/data/yearly_sub_match_results.RDS")

ndc_p_res_by_yr <- readRDS("~/git/business_innovation/datayearly_prop_match_results.RDS")
ndc_np_res_by_yr <- readRDS("~/git/business_innovation/data/yearly_nonprop_match_results.RDS")
ndc_s_res_by_yr <- readRDS("~/git/business_innovation/data/yearly_sub_match_results.RDS")





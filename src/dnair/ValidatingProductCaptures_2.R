library(dplyr)
library(stringr)

ndc_product <- readr::read_tsv("~/git/business_innovation/data/original/NDC/product.txt")
final_product_list <- readRDS("~/git/business_innovation/data/working/sec/finalwordlists/final_product_list_dec20.RDS")
final_product_list_singlefirstmentionsONLY <- readRDS("~/git/business_innovation/data/working/sec/finalwordlists/final_product_list_singmention_dec20.RDS")


ndc_names <- ndc_product %>% 
  mutate(year = str_extract(string = STARTMARKETINGDATE, pattern = "^\\d{4}")) %>% 
  select(21, 4, 6, 14) %>% 
  distinct() %>% 
  mutate(Prop_low = str_to_lower(PROPRIETARYNAME), 
         Nonprop_low = str_to_lower(NONPROPRIETARYNAME),
         Sub_low = str_to_lower(SUBSTANCENAME))

captures <- final_product_list_singlefirstmentionsONLY %>% select(first_mention, Token, Name )

captures_patt <- paste0(paste("\\b",str_trim(str_replace_all(str_remove_all(str_to_lower(unique(captures$Token)), "\\(|\\)"), "\\\u0097|\\\u0093", " ")), "\\b", sep = ""), collapse = "|")

library(data.table)
library(maditr)

# prop_results <- ndc_names %>% as.data.table() %>%
#   dt_select(year, PROPRIETARYNAME, Prop_low) %>% 
#   dt_mutate(prop_match = str_extract_all(Prop_low, pattern = captures_patt), 
#             matches = lengths(prop_match))

nonprop_results <- ndc_names %>% 
  select(year, NONPROPRIETARYNAME, Nonprop_low) %>% 
  mutate(nonprop_match = str_extract_all(Nonprop_low, pattern = captures_patt),
         matches = lengths(nonprop_match))

sub_results <- ndc_names %>% 
  select(year, SUBSTANCENAME, Sub_low) %>% 
  mutate(sub_match = str_extract_all(Sub_low, pattern = captures_patt), 
         matches = lengths(sub_match))

library(dplyr)
library(stringr)

datapath <- "/project/biocomplexity/sdad/projects-active/volume_nyc1_01/business_innovation/" #new rivanna location
ndc_product <-  readr::read_tsv(paste0(datapath, "original/NDC/product.txt"))
final_product_list <- readRDS(paste0(datapath, "working/sec/finalwordlists/final_product_list_dec20.RDS"))
final_product_list_singlefirstmentionsONLY <- readRDS(paste0(datapath, "working/sec/finalwordlists/final_product_list_singmention_dec20.RDS"))

ndc_names <- ndc_product %>%
  mutate(year = str_extract(string = STARTMARKETINGDATE, pattern = "^\\d{4}")) %>%
  select(21, 4, 6, 14) %>%
  distinct() %>%
  mutate(Prop_low = str_to_lower(PROPRIETARYNAME),
         Nonprop_low = str_to_lower(NONPROPRIETARYNAME),
         Sub_low = str_to_lower(SUBSTANCENAME))

captures <- final_product_list_singlefirstmentionsONLY %>% select(first_mention, Token, Name )

captures_patt <- paste0("\\b", "(", paste0( paste(str_trim(str_replace_all(str_remove_all(str_to_lower(unique(captures$Token)), "\\(|\\)"), "\\\u0097|\\\u0093", " ")), sep = ""), collapse = "|"), ")", "\\b")

library(data.table)
library(maditr)

### ALL TIME

prop_results <- ndc_names %>% as.data.table() %>%
 dt_select(year, PROPRIETARYNAME, Prop_low) %>%
 dt_mutate(prop_match = str_extract_all(Prop_low, pattern = captures_patt),
           matches = lengths(prop_match))

# saveRDS(prop_results, "git/business_innovation/data/ndc_prop_match_results.RDS")

nonprop_results <- ndc_names %>%  as.data.table() %>%
  dt_select(year, NONPROPRIETARYNAME, Nonprop_low) %>%
  dt_mutate(nonprop_match = str_extract_all(Nonprop_low, pattern = captures_patt),
         matches = lengths(nonprop_match))

# saveRDS(nonprop_results, "git/business_innovation/data/ndc_nonprop_match_results.RDS")

sub_results <- ndc_names %>% as.data.table() %>%
  dt_select(year, SUBSTANCENAME, Sub_low) %>%
  dt_mutate(sub_match = str_extract_all(Sub_low, pattern = captures_patt),
         matches = lengths(sub_match))

# saveRDS(sub_results, "git/business_innovation/data/ndc_sub_match_results.RDS")


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





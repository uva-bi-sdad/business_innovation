library(dplyr)
library(stringr)

datapath1 <- "/project/biocomplexity/sdad/projects_data/ncses/bi/binn/"
ndc_product <-  readr::read_tsv(paste0(datapath1, "original/NDC/product.txt"))
final_product_list <- readRDS(paste0(datapath1, "working/sec/finalwordlists/final_product_list_dec20.RDS"))
final_product_list_singlefirstmentionsONLY <- readRDS(paste0(datapath1, "working/sec/finalwordlists/final_product_list_singmention_dec20.RDS"))

ndc_sec_ref <- readRDS(paste0(datapath1, "working/NDC/ndc_sec_ref90.RDS"))

ndc_names1 <- ndc_product %>%
  filter(LABELERNAME %in% ndc_sec_ref$NDCCompany) %>% 
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

prop_results1 <- ndc_names1 %>% as.data.table() %>%
  dt_select(year, PROPRIETARYNAME, Prop_low) %>%
  dt_mutate(prop_match = str_extract_all(Prop_low, pattern = captures_patt),
            matches = lengths(prop_match))

#saveRDS(prop_results, paste0(datapath1, "working/NDC/ndc_prop_match_results1.RDS"))

nonprop_results1 <- ndc_names1 %>%  as.data.table() %>%
  dt_select(year, NONPROPRIETARYNAME, Nonprop_low) %>%
  dt_mutate(nonprop_match = str_extract_all(Nonprop_low, pattern = captures_patt),
            matches = lengths(nonprop_match))

#saveRDS(nonprop_results, paste0(datapath1, "working/NDC/ndc_nonprop_match_results1.RDS")) 

sub_results1 <- ndc_names1 %>% as.data.table() %>%
  dt_select(year, SUBSTANCENAME, Sub_low) %>%
  dt_mutate(sub_match = str_extract_all(Sub_low, pattern = captures_patt),
            matches = lengths(sub_match))

#saveRDS(sub_results, paste0(datapath1, "working/NDC/ndc_sub_match_results1.RDS"))



prop_results <- readRDS(paste0(datapath1, "working/NDC/ndc_prop_match_results1.RDS"))
nonprop_results <- readRDS(paste0(datapath1, "working/NDC/ndc_nonprop_match_results1.RDS"))
sub_results <- readRDS(paste0(datapath1, "working/NDC/ndc_sub_match_results1.RDS"))

table(prop_results$matches)
table(nonprop_results$matches)
table(sub_results$matches)

# OVERALL - without time matching
prop_matching_results1 <- prop_results1 %>% count(year, matches) %>%
  filter(as.integer(year) > 2010) %>%
  reshape2::dcast(year~matches, fun.aggregate = sum, value.var = "n") %>%
  transmute(year = year,
            products_unmatched = `0`,
            products_matched = `1`+`2`,
            perc_matched = paste0(as.character(as.integer(100*(products_matched/(products_unmatched + products_matched)))), "%", sep = " "))

nonprop_matching_results1 <- nonprop_results1 %>% count(year, matches) %>%
  filter(as.integer(year) > 2010) %>%
  reshape2::dcast(year~matches, fun.aggregate = sum, value.var = "n") %>%
  transmute(year = year,
            products_unmatched = `0`,
            products_matched = `1`+`2`+`3`,
            perc_matched = paste0(as.character(as.integer(100*(products_matched/(products_unmatched + products_matched)))), "%", sep = " "))

sub_matching_results1 <- sub_results1 %>% count(year, matches) %>%
  filter(as.integer(year) > 2010) %>%
  reshape2::dcast(year~matches, fun.aggregate = sum, value.var = "n") %>%
  transmute(year = year,
            products_unmatched = `0`,
            products_matched = `1`+`2`,
            perc_matched = paste0(as.character(as.integer(100*(products_matched/(products_unmatched + products_matched)))), "%", sep = " "))

prop_matching_results1 <- prop_matching_results1 %>% mutate(match = "prop")
nonprop_matching_results1 <-nonprop_matching_results1 %>% mutate(match = "nonprop")
sub_matching_results1 <- sub_matching_results1 %>% mutate(match = "sub")

matching_results1 <- rbind(prop_matching_results1, nonprop_matching_results1, sub_matching_results1)

ggplot(data = matching_results, aes(x = year, y = perc_matched, fill = match)) +
  geom_bar(stat = "identity", position = "dodge")











# ### Year specific
# 
# 
# ndc <- ndc_names %>% filter(year > 2011 & year < 2018)
# ndc_by_yr <- split(x = ndc, f = ndc$year )
# captures_by_yr <- split(captures, f = captures$first_mention)
# 
# library(data.table)
# library(maditr)
# 
# patt <- list()
# 
# for (i in 1:length(captures_by_yr)) {
#   capts <- unique(captures_by_yr[[i]]$Token)
#   patt[i] <- paste0("\\b", "(", paste0( paste(str_trim(str_replace_all(str_remove_all(str_to_lower(capts), "\\(|\\)"), "\\\u0097|\\\u0093", " ")), sep = ""), collapse = "|"), ")", "\\b")
#   patt <- unlist(patt)
# }
# 
# ndc_p_res_by_yr <- list()
# ndc_np_res_by_yr <- list()
# ndc_s_res_by_yr <- list()
# 
# for (i in 1:length(ndc_by_yr)) {
#   ndc_p_res_by_yr[[i]] <- ndc_by_yr[[i]] %>% as.data.table() %>%
#     dt_select(year, PROPRIETARYNAME, Prop_low) %>%
#     dt_mutate(prop_match = str_extract_all(Prop_low, pattern = patt[i]),
#               matches = lengths(prop_match))
#   
#   ndc_np_res_by_yr[[i]] <- ndc_by_yr[[i]] %>% as.data.table() %>%
#     dt_select(year, PROPRIETARYNAME, Prop_low) %>%
#     dt_mutate(prop_match = str_extract_all(Prop_low, pattern = patt[i]),
#               matches = lengths(prop_match))
#   
#   ndc_s_res_by_yr[[i]] <- ndc_by_yr[[i]] %>% as.data.table() %>%
#     dt_select(year, PROPRIETARYNAME, Prop_low) %>%
#     dt_mutate(prop_match = str_extract_all(Prop_low, pattern = patt[i]),
#               matches = lengths(prop_match))
# }
# 
# saveRDS(ndc_p_res_by_yr, "~/git/business_innovation/ yearly_prop_match_results.RDS")
# saveRDS(ndc_np_res_by_yr, "~/git/business_innovation/data/yearly_nonprop_match_results.RDS")
# saveRDS(ndc_s_res_by_yr, "~/git/business_innovation/data/yearly_sub_match_results.RDS")
# 
# ndc_p_res_by_yr <- readRDS("~/git/business_innovation/datayearly_prop_match_results.RDS")
# ndc_np_res_by_yr <- readRDS("~/git/business_innovation/data/yearly_nonprop_match_results.RDS")
# ndc_s_res_by_yr <- readRDS("~/git/business_innovation/data/yearly_sub_match_results.RDS")
# 




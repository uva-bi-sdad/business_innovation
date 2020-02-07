
datapath <- "/project/biocomplexity/sdad/projects_data/ncses/bi/" #new rivanna location


prop_results <- readRDS(paste0(datapath, "ndc_prop_match_results.RDS"))
nonprop_results <- readRDS(paste0(datapath, "ndc_nonprop_match_results.RDS"))
sub_results <- readRDS(paste0(datapath, "ndc_sub_match_results.RDS"))

table(sub_results$matches)
table(prop_results$matches)

# OVERALL - without time matching
prop_matching_results <- prop_results %>% count(year, matches) %>%
  filter(as.integer(year) > 2010) %>%
  reshape2::dcast(year~matches, fun.aggregate = sum, value.var = "n") %>%
  transmute(year = year,
            products_unmatched = `0`,
            products_matched = `1`+`2`+`3`+`4`+`5`,
            perc_matched = paste0(as.character(as.integer(100*(products_matched/(products_unmatched + products_matched)))), "%", sep = " "))

nonprop_matching_results <- nonprop_results %>% count(year, matches) %>%
  filter(as.integer(year) > 2010) %>%
  reshape2::dcast(year~matches, fun.aggregate = sum, value.var = "n") %>%
  transmute(year = year,
            products_unmatched = `0`,
            products_matched = `1`+`2`+`3`+`4`+`5`,
            perc_matched = paste0(as.character(as.integer(100*(products_matched/(products_unmatched + products_matched)))), "%", sep = " "))

sub_matching_results <- sub_results %>% count(year, matches) %>%
  filter(as.integer(year) > 2010) %>%
  reshape2::dcast(year~matches, fun.aggregate = sum, value.var = "n") %>%
  transmute(year = year,
            products_unmatched = `0`,
            products_matched = `1`+`2`+`3`+`4`+`5`,
            perc_matched = paste0(as.character(as.integer(100*(products_matched/(products_unmatched + products_matched)))), "%", sep = " "))

prop_matching_results <- prop_matching_results %>% mutate(match = "prop")
nonprop_matching_results <-nonprop_matching_results %>% mutate(match = "nonprop")
sub_matching_results <- sub_matching_results %>% mutate(match = "sub")

matching_results <- rbind(prop_matching_results, nonprop_matching_results, sub_matching_results)

ggplot(data = matching_results, aes(x = year, y = perc_matched, fill = match)) +
  geom_bar(stat = "identity", position = "dodge")

# YEAR TO YEAR - with time matching

prop_matches <- prop_results %>%
  left_join(prop_results %>% tidyr::unnest(), by = c("year", "PROPRIETARYNAME", "Prop_low", "matches"))

captures$token_clean <- str_trim(str_replace_all(str_remove_all(str_to_lower(captures$Token), "\\(|\\)"), "\\\u0097|\\\u0093", " "))
# removeme <- captures %>% count(first_mention, Name, token_clean) %>% select(-n) %>% count(first_mention, token_clean) %>% filter(n>1)
# captures <- captures %>% filter(token_clean %!in% removeme$token_clean)

prop_matches$year <- as.numeric(prop_matches$year)
captures

innjoin <- prop_matches %>%
  inner_join(captures, by = c("year" = "first_mention", "prop_match.y" = "token_clean"))

antjoin <- prop_matches %>%
  anti_join(captures, by = c("year" = "first_mention", "prop_match.y" = "token_clean"))

antjoin <- antjoin %>% filter(year > 2011 & year < 2018)

table(innjoin$year)/table(antjoin$year)

barplot(as.integer(100*(table(innjoin$year)/table(antjoin$year))))
table(innjoin$year)
table(antjoin$year)



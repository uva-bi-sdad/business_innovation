library(dplyr)

#datapath <- "/project/biocomplexity/sdad/projects-active/volume_nyc1_01/business_innovation/" #new rivanna location
datapath <- "/project/biocomplexity/sdad/projects_data/ncses/bi/binn/" #new rivanna location
dna_cmda_hlbody <- readr::read_csv(paste0(datapath, "working/DNA_Aggregated/CMDA_FALL2019/CMDA_FALL2019_hlwbody_hlbasedlabel.csv"))
dna_cmda_hlbody <- dna_cmda_hlbody %>% transmute(id, label = recode(innovYN, `No` = 0, `no` = 0, `Yes` = 1, `yes` = 1), hl = title, text = paste0(title, body))
dna_cmda_hlbody

# stem_hunspell <- function(term) {
#   # look up the term in the dictionary
#   stems <- hunspell::hunspell_stem(term)[[1]]
#
#   if (length(stems) == 0) { # if there are no stems, use the original term
#     stem <- term
#   } else { # if there are multiple stems, use the last one
#     stem <- stems[[length(stems)]]
#   }
#
#   stem
# }

dna_cmda_body <- dna_cmda_hlbody %>%
  mutate(hl = stringr::str_trim(stringr::str_replace_all(str_remove_all(text, "\\(|\\)"), "[:punct:]", " ")),
         text_tokens = stringr::str_split(string = hl, pattern = " ")) %>%
  tidyr::unnest(text_tokens) %>%
  mutate(brand = str_extract(text_tokens, "®|™"),
         text_tokens = str_remove_all(str_trim(text_tokens), "®|™")) %>%    # filter(str_detect(text_tokens, "®|™")) # %>% select(text) %>% View()
  filter(text_tokens != "^\\s*$") %>%
  mutate(eng = hunspell::hunspell_check(text_tokens),
         caps = stringr::str_detect(text_tokens, pattern = "^[[:upper:]]"),
         stem = ifelse(eng == TRUE & caps == FALSE, 1, 0)) %>%
  filter(!dataplumbr::var.is_blank(text_tokens))

# for (i in 1:nrow(dna_cmda_body)) {
#   dna_cmda_body$stem_w[i] <- ifelse(dna_cmda_body$stem[i] == 1, stem_hunspell(dna_cmda_body$text_tokens[i]), dna_cmda_body$text_tokens[i])
# }
#
# dna_cmda_body %>% count(stem_w) %>% arrange(desc(n))
#
# test <- dna_cmda_hlbody %>% select(1,2,9)
#
# test_dfm <- tidytext::cast_dfm(test, document = id, term = stem_w, value = label)


dna_cmda_body_nested_all <- dna_cmda_body %>% select(id, label, text_tokens, brand, eng) %>% tidyr::nest(cols = c("text_tokens", "eng", "brand"))
candidateproducts <- dna_cmda_body %>% select(id, label, text_tokens, brand, eng) %>% filter(!is.na(brand)|eng == FALSE)




library(dplyr)

datapath <- "/project/biocomplexity/sdad/projects-active/volume_nyc1_01/business_innovation/" #new rivanna location
dna_cmda_hlbody <- readr::read_csv(paste0(datapath, "working/DNA_Aggregated/CMDA_FALL2019/CMDA_FALL2019_hlwbody_hlbasedlabel.csv"))
dna_cmda_hlbody <- dna_cmda_hlbody %>% transmute(id, label = recode(innovYN, `No` = 0, `no` = 0, `Yes` = 1, `yes` = 1), hl = title, text = paste0(title, body))
dna_cmda_hlbody

stem_hunspell <- function(term) {
  # look up the term in the dictionary
  stems <- hunspell::hunspell_stem(term)[[1]]
  
  if (length(stems) == 0) { # if there are no stems, use the original term
    stem <- term
  } else { # if there are multiple stems, use the last one
    stem <- stems[[length(stems)]]
  }
  
  stem
}

dna_cmda_body <- dna_cmda_hlbody %>% 
  mutate(hl = stringr::str_trim(stringr::str_replace_all(text, "[:punct:]", " ")), 
         text_tokens = stringr::str_split(string = hl, pattern = "\\b")) %>% 
  tidyr::unnest(text_tokens) %>% 
  filter(!dataplumbr::var.is_blank(text_tokens)) %>%
  filter(text_tokens != " " & text_tokens != "  " & text_tokens != "   ") %>% 
  mutate(eng = hunspell::hunspell_check(text_tokens), 
         caps = stringr::str_detect(text_tokens, pattern = "^[[:upper:]]"), 
         stem = ifelse(eng == TRUE & caps == FALSE, 1, 0))

for (i in 1:nrow(dna_cmda_body)) {
  dna_cmda_body$stem_w[i] <- ifelse(dna_cmda_body$stem[i] == 1, stem_hunspell(dna_cmda_body$text_tokens[i]), dna_cmda_body$text_tokens[i])
}

dna_cmda_body %>% count(stem_w) %>% arrange(desc(n))

test <- dna_cmda_hlbody %>% select(1,2,9)

test_dfm <- tidytext::cast_dfm(test, document = id, term = stem_w, value = label)




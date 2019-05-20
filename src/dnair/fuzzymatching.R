
library(readr)
wordcounts_1_1000 <- read_csv("data/business_innovation/working/sec/wordcounts_1_1000.csv")
wordcounts_1001_2000 <- read_csv("data/business_innovation/working/sec/wordcounts_1001_2000.csv")
wordcounts_2000_2867 <- read_csv("data/business_innovation/working/sec/wordcounts_2000_2867.csv")
allwordcounts <- rbind(wordcounts_1_1000, wordcounts_2000_2867, wordcounts_2000_2867)
cik_ticker <- read_delim("data/business_innovation/original/edgar_filings/cik_ticker.csv", delim = "|")
sic <- read_rds("data/business_innovation/original/sic.download.RDS")
ciknames <- read_rds("data/business_innovation/original/ciks_names.RDS")

wcbycomp <- reshape2::dcast(allwordcounts, Company + Words ~ Year, sum)
wcbyword <- reshape2::dcast(allwordcounts, Words + Company ~ Year, sum)

#wcbyword %>% group_by(Words, Company) %>% summarise(count1 = n()) %>% select(-count1) %>% group_by(Words) %>% summarise(count = n()) %>% filter(count == 1)

head(wcbycomp)
head(wcbyword)
head(cik_ticker)

sic$SIC <- as.numeric(sic$SIC)
sic$CIK <- as.numeric(sic$CIK)
ciknames$cik <- as.numeric(ciknames$cik)
ciknames$sic <- as.numeric(ciknames$sic)
incorp <- cik_ticker %>% select(CIK, Incorporated)

wcbycompdetails <- wcbycomp %>%
  left_join(ciknames, by = c("Company" = "cik")) %>%
  left_join(sic, by = c("Company" = "CIK")) %>%
  left_join(cik_ticker, by = c("Company" = "CIK"))

colnames(wcbycompdetails) <- c("CIK", "Token", as.character(2012:2017), "SEC_Company_Name", "SEC_SIC", "SIC_Company_Name", "SIC_SIC", "SIC_Industry", "SIC_Location", "Ticker_Code", "Ticker_Company_Name", "Ticker_Exchange", "Ticker_SIC_Code", "Ticker_Location", "Ticker_IncLoc", "Ticker_IRS")

###### COMPANY NAMES #########

library(stringr)
library(dplyr)
library(corpus)

###### PORTER STEMMING FUNCTION #########

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

###### REFERENCE COMPANIES #########
companylist <- unique(ciknames$company_names) #  unique(wcbycompdetails$Name)

findstops <- companylist %>%
  str_remove(pattern = "[[:punct:]]") %>%
  str_to_lower() %>%
  str_split(pattern = " ") %>%
  unlist() %>%
  str_remove(pattern = "[[:punct:]]") %>%
  table() %>%
  as.data.frame() %>%
  `colnames<-`(c("word", "Freq")) %>%
  arrange(desc(Freq)) %>%
  filter(nchar(as.character(word)) >= 2) %>%
  filter(Freq > 1)

findstops
stopwords <- as.vector(c("inc", "corp", "ltd","plc","llc","hold?ing?s","international","group","acquisition","american","china","usa"))
stopwords <- paste0( stopwords, collapse = "|")

pharmstopwords <- c("biopharma", "therapeutics?", "pharmaceuticals?", "international", "sciences?", "medical", "technology", "phrma", "pharma", "bio", "biosciences?")
pharmstopwords <- paste0(paste0("\\b", pharmstopwords, "\\b"), collapse = "|")


comp_tokens <- str_split(companylist, pattern = " |[[:punct:]]")

new_ref_companies <- tibble(companylist, comp_tokens) %>%
  tidyr::unnest() %>%
  filter(nchar(comp_tokens) > 3) %>%
  mutate(
    comp_lowword = comp_tokens %>%
      #str_remove_all(pattern = "[[:punct:]]") %>%
      str_to_lower() %>%
      #str_remove_all(pattern = businesstops) %>%
      #str_remove_all(pattern = testpatt) %>%
      str_squish()
    , comp_low_hun = text_tokens(new_ref_companies$comp_lowword, stemmer = stem_hunspell) %>% unlist()
  )

## do this to new_ref_companies - LATER!! after the hunspell THEN filter out hunspell column based on stop words
## str_remove_all(pattern = stopwords) %>% str_remove_all(pattern = pharmstopwords) %>%

topwords <- new_ref_companies %>% group_by(comp_low_hun) %>% summarise(count = n()) %>% arrange(desc(count))
candidate_pharm_stopwords <- topwords %>% filter(count > 1) %>% mutate(eng = hunspell_check(comp_low_hun, dict = "en_US")) %>% filter(eng == TRUE) %>% select(comp_low_hun)
pharmstopwords <- paste0(paste0("\\b", candidate_pharm_stopwords$comp_low_hun, "\\b"), collapse = "|")

new_ref_companies <- new_ref_companies %>% mutate(pharmstop = grepl(x = comp_low_hun, pattern = pharmstopwords))


new_ref_companies %>% filter(pharmstop == TRUE) %>% group_by(comp_tokens) %>% summarise(count = n())
new_ref_companies %>% filter(pharmstop == FALSE) %>% group_by(comp_tokens) %>% summarise(count = n())

#length(unique(ref_companies$comp_tokens))
# dupecomptokens <- ref_companies$comp_tokens[which(duplicated(ref_companies$comp_tokens) == TRUE)] %>% as.data.frame()
# multimatch <- dupecomptokens %>% `colnames<-`(c("word")) %>% left_join(ref_companies, by = c("word" = "comp_tokens"))
# View(multimatch)
# multimatch %>% group_by(word) %>% summarise(count = n()) %>% arrange(desc(count))

###### STRINGS TO TEST  #########
stringtest <- unique(wcbycompdetails$Token)

cleanstring <- stringtest %>%
  str_remove_all(pattern = "[[:punct:]]") %>%
  str_to_lower() %>%
  str_remove_all(pattern = stopwords) %>%
  str_trim()

cleanstring

#ref_companies <- ref_companies %>% filter(nchar(comp_tokens) > 3)
#ref_companies <- ref_companies %>% filter(hunspell::hunspell_check(comp_tokens) == FALSE)
#ref_companies %>% group_by(comp_tokens) %>% summarise(count = n()) %>% arrange((count))

#tofind <- paste(paste0("[:space:]", ref_companies$comp_tokens, "[:space:]"), collapse="|")
tofind <- paste(paste0("^", ref_companies$comp_tokens, "$"), collapse="|")
results <- str_extract(string = cleanstring, pattern = tofind)
results2 <- str_extract(string = stringtest, pattern = tofind)

#ref_companies_unique <- ref_companies %>% group_by(comp_tokens) %>% summarise(count = n()) %>% filter(count == 1) %>% select(-count) %>%
#  left_join(ref_companies, by = "comp_tokens") %>% as.data.table()
companyfind <- tibble::tibble("Token" = cleanstring, "Comp_lowword_Match" = results, "Comp_Token_Match" = results2)
wcbycompdetails$lowword <- str_to_lower(wcbycompdetails$Token)

wcbycompdetails_compcheck <-
wcbycompdetails %>%  left_join(companyfind, by = c("lowword" = "Token"))
companyfind %>% left_join(ref_companies, by = c("Token" = "comp_tokens")) %>% filter(!is.na(companylist))

wcbycompdetails_compcheck %>% mutate(
  SEC_name_match = as.numeric(mapply(grepl, wcbycompdetails_compcheck$Token, wcbycompdetails_compcheck$SEC_Company_Name)),
  SIC_name_match = as.numeric(mapply(grepl, wcbycompdetails_compcheck$Token, wcbycompdetails_compcheck$SIC_Company_Name)),
  Ticker_name_match = as.numeric(mapply(grepl, wcbycompdetails_compcheck$Token, wcbycompdetails_compcheck$Ticker_Company_Name)),
  Self = (SEC_name_match + SIC_name_match + Ticker_name_match),
  Comp = ifelse(Self == 0, 1, 0) ) %>%
  #select(-SEC_name_match, -SIC_name_match, -Ticker_name_match) %>%
  filter(companylist != "<NA>")

head(comp_tokens)
head(comp_lowwords)

# saveRDS(wcbycomp, "data/business_innovation/working/sec/fuzzymatching/1_wcbycomp.RDS")
# saveRDS(wcbyword, "data/business_innovation/working/sec/fuzzymatching/1_wcbyword.RDS")
# saveRDS(wcbycompdetails, "data/business_innovation/working/sec/fuzzymatching/2_wcbycompdetails.RDS")
# saveRDS(wcbycomp_mincomp, "data/business_innovation/working/sec/fuzzymatching/3_wcbycomp_mincomp.RDS")
# saveRDS(wcbycomp_comp, "data/business_innovation/working/sec/fuzzymatching/3_wcbycomp_actcomp.RDS")

#saveRDS(wcbycomp_mincomp, "data/business_innovation/working/sec/fuzzymatching/3_wcbycomp_mincomp.RDS")
wcbycomp_comp <- readRDS("data/business_innovation/working/sec/fuzzymatching/3_wcbycomp_actcomp.RDS")
View(wcbycomp_comp)
########################################################################

comptokensidentified <- wcbycompdetails_compcheck %>% filter(!dataplumbr::is_blank(Comp_Token_Match)) %>% select(CIK, Token, SEC_Company_Name, SIC_Company_Name, Ticker_Company_Name, lowword, Comp_Token_Match, companylist)

comptokensidentified[20:25,]

comptokensidentified <- comptokensidentified %>% mutate(
  SEC_name_match = as.numeric(mapply(grepl, comptokensidentified$Token, comptokensidentified$SEC_Company_Name)),
  SIC_name_match = as.numeric(mapply(grepl, comptokensidentified$Token, comptokensidentified$SIC_Company_Name)),
  Ticker_name_match = as.numeric(mapply(grepl, comptokensidentified$Token, comptokensidentified$Ticker_Company_Name)),
  Self = (SEC_name_match + SIC_name_match + Ticker_name_match),
  Comp = ifelse(Self == 0, 1, 0) ) %>%
  select(-SEC_name_match, -SIC_name_match, -Ticker_name_match)



lapply(X = comptokensidentified, FUN =  grepl(x = comptokensidentified$Token, pattern = comptokensidentified$SEC_Company))

mapply(grepl,pat,text)

SEC_match <- mapply(grepl, comptokensidentified$Token, comptokensidentified$SEC_Company_Name)





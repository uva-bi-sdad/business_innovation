
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
  left_join(sic, by = c("Company" = "CIK", "sic" = "SIC")) %>%
  left_join(incorp, by = c("Company" = "CIK"))


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
pharmstopwords <- c("biopharma|therapeutics?|||pharmaceuticals?|international|science|medical|technology|phrma|pharma|bio")
pharmstopwords <- c("biopharma", "therapeutics?", "pharmaceuticals?", "international", "science", "medical", "technology", "phrma", "pharma", "bio")
pharmstopwords <- paste0(paste0("\\b", pharmstopwords, "\\b"), collapse = "|")

# testpatt <- paste0(paste0(paste0("(?<=\\s)", findstops$word,"?\\s"), collapse = "|"),
#               paste0(paste0("(?<=[:space:])", findstops$word,"?\\s"), collapse = "|"),
#               collapse = "|")
#
# testpatt <- paste0(paste0("[:space:]", findstops$word,"?\\s"), collapse = "|")
#businesstops <- paste0(paste0("\\s", stopwords, "?\\w"), collapse = "|")
#

# pharmstopwords <- c("pharma\\W||therapeutic|pharmaceutical|international|science|medical|tech?nolog(y|ies)|phrma|health|diagnostic|tech|advance?d|care|system")
#
# pharmstopwords1 <- c("therapeutic", "pharmaceutical")
#
# pharmstopwords2 <- c("bio\\s")
#
# test <- c("BIO PATH HOLDINGS INC", "PRANA BIOTECHNOLOGY LTD", "ACHILLION PHARMACEUTICALS INC")
# str_remove_all(string = str_to_lower(("ACHILLION PHARMACEUTICALS INC")), pattern = testpatt)
#
# str_remove_all(str_remove_all(string = str_to_lower(test), pattern = businesstops), pattern = testpatt)  # businesstops)

refcompanies <- companylist %>%
  str_remove_all(pattern = "[[:punct:]]") %>%
  str_to_lower() %>%
  #str_remove_all(pattern = businesstops) %>%
  #str_remove_all(pattern = testpatt) %>%
  str_remove_all(pattern = stopwords) %>%
  str_remove_all(pattern = pharmstopwords) %>%
  str_squish()

comp_tokens <- text_tokens(refcompanies, stemmer = stem_hunspell)

ref_companies <- tidyr::unnest(tibble::tibble(companylist, refcompanies, comp_tokens))  %>% filter(nchar(comp_tokens) > 2)

#length(unique(ref_companies$comp_tokens))
# dupecomptokens <- ref_companies$comp_tokens[which(duplicated(ref_companies$comp_tokens) == TRUE)] %>% as.data.frame()
# multimatch <- dupecomptokens %>% `colnames<-`(c("word")) %>% left_join(ref_companies, by = c("word" = "comp_tokens"))
# View(multimatch)
# multimatch %>% group_by(word) %>% summarise(count = n()) %>% arrange(desc(count))

###### STRINGS TO TEST  #########
stringtest <- unique(wcbycompdetails$Words)

cleanstring <- stringtest %>%
  str_remove_all(pattern = "[[:punct:]]") %>%
  str_to_lower() %>%
  str_remove_all(pattern = stopwords) %>%
  str_trim()

cleanstring

#ref_companies <- ref_companies %>% filter(nchar(comp_tokens) > 3)
ref_companies <- ref_companies %>% filter(hunspell::hunspell_check(comp_tokens) == FALSE)
ref_companies %>% group_by(comp_tokens) %>% summarise(count = n()) %>% arrange((count))

#tofind <- paste(paste0("[:space:]", ref_companies$comp_tokens, "[:space:]"), collapse="|")
tofind <- paste(paste0("^", ref_companies$comp_tokens, "$"), collapse="|")
cleanstring <- str_extract(string = cleanstring, pattern = tofind)

ref_companies_unique <- ref_companies %>% group_by(comp_tokens) %>% summarise(count = n()) %>% filter(count == 1) %>% select(-count) %>%
  left_join(ref_companies, by = "comp_tokens") %>% as.data.table()
companyfind <- tibble::tibble(cleanstring, results)
wcbycompdetails$lowword <- str_to_lower(wcbycompdetails$Words)

wcbycompdetails %>% left_join(ref_companies %>% filter(count == 1), by = c("lowword" = "comp_tokens"))

wcbycomp_mincomp <- wcbycompdetails %>%  left_join(companyfind, by = c("lowword" = "cleanstring")) %>% filter(is.na(results)) %>% arrange(Words)
wcbycomp_comp <- wcbycompdetails %>%  left_join(companyfind, by = c("lowword" = "cleanstring")) %>% filter(!is.na(results)) %>% arrange(Words)

View(wcbycomp_mincomp)
View(wcbycomp_comp)

head(ref_companies, 7)

# saveRDS(wcbycomp, "data/business_innovation/working/sec/fuzzymatching/1_wcbycomp.RDS")
# saveRDS(wcbyword, "data/business_innovation/working/sec/fuzzymatching/1_wcbyword.RDS")
# saveRDS(wcbycompdetails, "data/business_innovation/working/sec/fuzzymatching/2_wcbycompdetails.RDS")
# saveRDS(wcbycomp_mincomp, "data/business_innovation/working/sec/fuzzymatching/3_wcbycomp_mincomp.RDS")
# saveRDS(wcbycomp_comp, "data/business_innovation/working/sec/fuzzymatching/3_wcbycomp_actcomp.RDS")

#saveRDS(wcbycomp_mincomp, "data/business_innovation/working/sec/fuzzymatching/3_wcbycomp_mincomp.RDS")
wcbycomp_comp <- readRDS("data/business_innovation/working/sec/fuzzymatching/3_wcbycomp_actcomp.RDS")
View(wcbycomp_comp)
########################################################################

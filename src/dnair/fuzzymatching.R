
library(readr)
wordcounts_1_1000 <- read_csv("data/business_innovation/working/sec/wordcounts_1_1000.csv")
wordcounts_1001_2000 <- read_csv("data/business_innovation/working/sec/wordcounts_1001_2000.csv")
wordcounts_2000_2867 <- read_csv("data/business_innovation/working/sec/wordcounts_2000_2867.csv")
allwordcounts <- rbind(wordcounts_1_1000, wordcounts_2000_2867, wordcounts_2000_2867)
cik_ticker <- read_delim("data/business_innovation/original/edgar_filings/cik_ticker.csv", delim = "|")
sic <- read_rds("data/business_innovation/original/sic.download.RDS")
ciknames <- read_rds("data/business_innovation/original/ciks_names.RDS")
head(wordcounts_1_1000)

wcbycomp <- reshape2::dcast(allwordcounts, Company + Words ~ Year, sum)
wcbyword <- reshape2::dcast(allwordcounts, Words + Company ~ Year, sum)

wcbyword %>% group_by(Words, Company) %>% summarise(count1 = n()) %>% select(-count1) %>% group_by(Words) %>% summarise(count = n()) %>% filter(count == 1)

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
companylist <- unique(wcbycompdetails$Name)

findstops <- companylist %>%
  str_remove(pattern = "[[:punct:]]") %>%
  str_to_lower() %>%
  str_split(pattern = " ") %>%
  unlist() %>%
  str_remove(pattern = "[[:punct:]]") %>%
  table() %>%
  as.data.frame() %>%
  arrange(desc(Freq)) %>%
  head(n = 60L)

findstops
stopwords <- c("inc|corp|ltd|plc|llc|holdings")
pharmstopwords <- c("biopharma|therapeutic|pharmaceutical|international|science|medical|technology|phrma")

refcompanies <- companylist %>%
  str_remove_all(pattern = "[[:punct:]]") %>%
  str_to_lower() %>%
  str_remove_all(pattern = stopwords) %>%
  str_remove_all(pattern = pharmstopwords) %>%
  str_squish()


comp_tokens <- text_tokens(refcompanies, stemmer = stem_hunspell)

ref_companies <- tidyr::unnest(tibble::tibble(refcompanies, comp_tokens))
View(ref_companies)

###### STRINGS TO TEST  #########


stringtest <- unique(wcbycompdetails$Words)

cleanstring <- stringtest %>%
  str_remove_all(pattern = "[[:punct:]]") %>%
  str_to_lower() %>%
  str_remove_all(pattern = stopwords) %>%
  str_trim()

cleanstring

ref_companies <- ref_companies %>% filter(nchar(comp_tokens) > 3)
ref_companies <- ref_companies %>% filter(hunspell::hunspell_check(comp_tokens) == FALSE)

tofind <- paste(ref_companies$comp_tokens, collapse="|")
results <- str_extract(string = cleanstring, pattern = tofind)

companyfind <- tibble::tibble(cleanstring, results)
wcbycompdetails$lowword <- str_to_lower(wcbycompdetails$Words)

wcbycomp_mincomp <- wcbycompdetails %>%  left_join(companyfind, by = c("lowword" = "cleanstring")) %>% filter(is.na(results)) %>% arrange(Words)
wcbycomp_comp <- wcbycompdetails %>%  left_join(companyfind, by = c("lowword" = "cleanstring")) %>% filter(!is.na(results)) %>% arrange(Words)

View(wcbycomp_mincomp)
View(wcbycomp_comp)

########################################################################
stringtest[10:20]
tail(refcompanies)
refcompanies[]
match <- stringdist::ain(stringtest, refcompanies, max = 1)
View(tibble::tibble(stringtest,  match))

unique(test$Industry)
unique(test$Incorporated)
unique(test$Location)



industrydict <- c(
  "Recombinant",
  "Connor",
  "Endo", #??
  "Pharma",

)

companydict <- c(
  "Genentech",
  "AstraZeneca",
)



###### GARBAGE

#match <- stringdist::ain(stringtest, refcompanies, maxDist = 5)
View(tibble::tibble(stringtest,  match))

stringdist::ain("leia",c("uhura","leela"),maxDist=1)
stringdist::amatch("leia", c("uhura", "leela"), maxDist = 2)
stringr::str_match("leia", c("uhura", "leela", "leia match company"))
stringr::str_extract(pattern = "leia", string = c("uhura", "leela", "leia match company"))

stringdist::stringdist("leia", c("uhura", "leela")) # 4 2
stringdist::stringdist( c("uhura", "leela", "leia"), "leia") # 4 2 0
stringdist::stringdist( c("uhura", "leela", "leia"), c("uhura", "max")) # 0 5 4 # warning
match <- stringdist::stringdist(stringtest, refcompanies)
match <- stringr::str_extract(pattern = refcompanies[2], string = stringtest)
sum(!is.na(match))
length(match)





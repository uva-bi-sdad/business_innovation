library(readr)
library(readxl)
library(stringr)
library(dplyr)
library(fuzzyjoin)
library(quanteda)
library(tidytext)

cik_ticker <- read_delim("data/original/edgar_filings/cik_ticker.csv", delim = "|")
sic <- read_rds("data/original/sic.download.RDS")
ciknames <- read_rds("data/original/ciks_names.RDS")
fda_approvals <- read_excel("data/original/fda_drugs/Copy of FDA Database - COMBINED V2.xlsx")

sic$SIC <- as.numeric(sic$SIC)
sic$CIK <- as.numeric(sic$CIK)
ciknames$cik <- as.numeric(ciknames$cik)
ciknames$sic <- as.numeric(ciknames$sic)

# nrow(ciknames) #779
# nrow(sic) #58427
# nrow(cik_ticker) #13737

company_list <- sic %>%
  full_join(ciknames, by = c("CIK" = "cik")) %>%
  left_join(cik_ticker, by = c("CIK", "SIC"))

colnames(company_list) <- c(
  "SEC_CIK", "SIC_Company_Name", "SIC_Code", "SIC_Industry", "SIC_Location",
  "SEC_Company_Name", "SEC_SIC_Code",
  "Ticker_Code", "Ticker_Company_Name", "Ticker_Exchange", "Ticker_Location", "Ticker_Incorporated", "Ticker_IRS" )

inc_list <- c("inc", "corp", "corporation", "plc", "de", "co", "llc", "ltd", "com", "int", "and", "&", "us", "health", "holdings")
inc_patt <- paste0(
  c(paste0("\\s", inc_list, "\\s"),
    paste0("\\b", inc_list, "\\b"),
    paste0("\\s", c("NA", "na"), "\\s"),
    paste0("\\b", c("NA", "na")),
    paste0(c("NA", "na"), "\\b"), "&" ),
  collapse = "|")

head(company_list)

pharm_comp_names_only <- company_list %>% filter(SIC_Code == 2834|SIC_Code == 5047| SIC_Code == 8731) # %>% select(SIC_Company_Name, SEC_Company_Name, Ticker_Company_Name) %>% mutate(name_count = 3 - rowSums(is.na(.)))
fda <- unique(fda_approvals$Company)
fda_patt <- paste0(fda, collapse = "|")
pharm_comp_names_wfda <- pharm_comp_names_only %>% mutate(FDA_Name_Orig1 = str_extract(string = paste(SIC_Company_Name, SEC_Company_Name, Ticker_Company_Name), pattern = fda_patt)) #%>% filter(is.na(FDA_Name_Orig1))

pharm_comp_names_wfda <- pharm_comp_names_wfda %>% mutate(mashup_orig = paste(SIC_Company_Name, SEC_Company_Name, Ticker_Company_Name, FDA_Name_Orig1, sep = "; "),
                                                          mashup_clean = paste(tolower(str_remove_all(SIC_Company_Name, "[:punct:]")),
                                                                               tolower(str_remove_all(SEC_Company_Name, "[:punct:]")),
                                                                               tolower(str_remove_all(Ticker_Company_Name, "[:punct:]"))),
                                                          mashup_more_clean = str_trim(str_remove_all(str_remove_all(mashup_clean, inc_patt), "\\s.\\s|\\s..\\s"),
                                                                                       side = "right"))

fda_df <- fda %>% as.data.frame()
colnames(fda_df) <- c("FDA_Company_Name")
fda_df <- fda_df %>% mutate(nopunct = str_remove_all(FDA_Company_Name, "[.|,|(|)]"),
                            lower = tolower(nopunct),
                            lower_clean = str_trim(str_remove_all(str_remove_all(lower, pattern = inc_patt), "\\s.\\s|\\s..\\s"), "right"))


fda_tokens <- unlist(str_split(fda_df$lower_clean, "[ |-]"))
fda_patt_plain <- paste0( c(paste0("\\s", fda_tokens, "\\s"), paste0("\\b", fda_tokens, "\\b")), collapse = "|")
fda_patt_regex <- paste0( c(paste0("\\s", fda_df$lower_clean, "\\s"), paste0("\\b", fda_df$lower_clean, "\\b")), collapse = "|")
pharm_comp_names_wfda <- pharm_comp_names_wfda %>%
  mutate(FDAmatch1 = str_extract(mashup_more_clean, pattern = fda_patt_plain),
         FDAmatch2 = str_extract(mashup_more_clean, pattern = fda_patt_regex),
         FDAmatch3 = str_extract(mashup_clean, pattern = fda_patt_plain),
         FDAmatch4 = str_extract(mashup_clean, pattern = fda_patt_regex),
         FDAtogether = paste(FDAmatch1, FDAmatch2, FDAmatch3, FDAmatch4, sep = " "),
         FDA_Company_Name = str_remove_all(vapply(lapply(strsplit(FDAtogether, " "), unique), paste, character(1L), collapse = " "), "[\\s|\\b]NA[\\s|\\b]"))

##FDAmatch1 has 84 matches
##FDAmatch2 has 91 matches
##FDAmatch3 has 103 matches
##FDAmatch4 has 121 matches
##FDAtogether has 89 matches
##FDA_Company_Name has 126 matches
pharm_comp_names_wfda %>% filter(str_detect(FDA_Company_Name, "[:alnum:]") == TRUE & !is.na(FDA_Company_Name) & str_detect(FDA_Company_Name, "\\bNA") == FALSE)
pharm_comp_names_wfda %>% filter(str_detect(FDAtogether, "[:alnum:]") == TRUE & !is.na(FDAtogether) & str_detect(FDAtogether, "\\bNA") == FALSE)

# #  select(-FDAmatch1, -FDAmatch2, -FDAmatch3, -FDAmatch4, -FDAtogether) %>%
# #  filter(!is.na(FDA)|FDA != "NA")#
# test %>% filter(str_detect(FDA, " ") == FALSE)
# test$FDA <- str_replace_all(test$FDA, "\\bNA", "")
# test %>% filter(str_detect(FDA, " ") == FALSE) #%>% nrow()
# test %>% filter(str_detect(FDA, " ") == FALSE) %>% nrow()
# View(test)
# head(fda_df)

pharm_comp_names_wfda %>% anti_join(fda_df, )

fda_nomatch <- fda_df %>% anti_join(pharm_comp_names_wfda, by = c("FDA_Company_Name" = "FDA_Name_Orig1") )
fda_match <- fda_df %>% inner_join(pharm_comp_names_wfda, by = c("FDA_Company_Name" = "FDA_Name_Orig1") )


### ### ### ### ### ### ### ### ###
fda_bigrams <- tidytext::unnest_tokens(tbl = fda_tokens, input = fda_full_name, output = "bigrams", token = "ngrams", n = 2)

mashupnames %>% regex_inner_join(fda_bigrams, by = c("mashup_clean" = "bigrams"))
fda_bigrams %>% regex_inner_join(mashupnames, by = c("bigrams" = "mashup_clean"))

fda_patt2 <- paste0(fda_bigrams$bigrams, collapse = "|")
mashupnames %>% mutate(fda_bi_match = str_extract(mashup_clean, fda_patt2))


head(pharm_comp_names_wfda)
head(fda_tokens)

#fda_bigrams <- fda_tokens %>% mutate(tokens = tokens(fda_full_name), bigrams = quanteda::tokens_ngrams(tokens, n = 2, concatenator = " "))
#fda_bi <- unique(unlist(str_remove_all(fda_bigrams$bigrams, "[(|)]")))
# fda_patt2 <- paste0(fda_bi, collapse = "|")
# fda_patt2 <- paste0( c(paste0("\\s", fda_bi, "\\s"), paste0("\\b", fda_bi, "\\b")), collapse = "|")
# pharm_comp_names_wfda <- pharm_comp_names_wfda %>%
#   mutate(FDA_Name_OrigBi = str_match(string = paste(SIC_Company_Name, SEC_Company_Name, Ticker_Company_Name), pattern = fda_patt2))
# mashupnames$mashup_clean %>% regex_inner_join(fda_bi)
# table(pharm_comp_names_wfda$FDA_Name_OrigBi)
# pharm_comp_names_wfda %>% filter(FDA_Name_OrigBi == TRUE)

######################################################################################################
######################################################################################################
####

dna_companies <- read.csv(unz("data/DNACompanies.zip", "companies.csv"))
sample_dna <- dna_companies %>% filter(code %in% c("USFDA", "LILYE", "SCHPLO", "TRIMEL", "MEDINC", "BOSTS"))
sample_dna <- sample_dna %>% group_by(code, description) %>% summarise(n = n()) %>% select(-n)
sample_dna <- sample_dna %>% mutate(nopunct = str_remove_all(description, "[.|,]"), lower = tolower(nopunct))
sample_dna$text <- sample_dna$lower
dna_tokens <- sample_dna %>% corpus::text_tokens() %>% tibble(sample_dna$text)
dna_tokens <- tidyr::unnest(dna_tokens)

colnames(dna_tokens) <- c("dna_full_name", "dna_token")
dna_tokens_uniqueish <- dna_tokens %>% filter(str_detect(dna_token, inc_patt) == FALSE)

dna_tokens_uniqueish %>% regex_inner_join(pharm_comp_names_wfda, by = c("dna_full_name" ="mashup_more_clean"))

dna_patt_plain <- paste0( c(paste0("\\s", dna_tokens_uniqueish$dna_token, "\\s"), paste0("\\b", dna_tokens_uniqueish$dna_token, "\\b")), collapse = "|")
dna_patt_regex <- paste0( c(paste0("\\s", dna_tokens_uniqueish$dna_token, "\\s"), paste0("\\b", dna_tokens_uniqueish$dna_token, "\\b")), collapse = "|")

pharm_comp_names_wfda %>% mutate(dna_token = str_extract(mashup_more_clean, dna_patt_regex)) %>% filter(!is.na(dna_token))
######################################################################################################
#dna_simp_a <- dna_simp %>% filter(str_detect(string = code, "^A") == TRUE)
dna_simp_a <- dna_simp %>% head(1000)
dna_simp_a <- dna_simp_a %>% mutate(nopunct = str_remove_all(description, "[:punct:]"), lower = tolower(nopunct))
dna_simp_a$text <- dna_simp_a$lower
dna_tokens_a <- dna_simp_a %>% corpus::text_tokens()
dna_tokens_a <- dna_simp_a$text %>% tibble(dna_tokens_a)
dna_tokens_a <- tidyr::unnest(dna_tokens_a)
colnames(dna_tokens_a) <- c("dna_full_name", "dna_token")
dna_tokens_a <- dna_tokens_a %>% filter(str_detect(dna_token, inc_patt) == FALSE)

dna_companies <- read.csv(unz("data/DNACompanies.zip", "companies.csv"))
dna_companies$code <- forcats::fct_explicit_na(dna_companies$code)
dna_simp <- dna_companies %>% group_by(code, description) %>% summarise(n = n()) %>% select(-n)
dna_simp <- dna_simp %>% mutate(nopunct = str_remove_all(description, "[.|,]"), lower = tolower(nopunct))
dna_simp$text <- dna_simp$lower
dna_tokens <- dna_simp %>% corpus::text_tokens() %>% tibble(dna_simp$text)
dna_tokens <- tidyr::unnest(dna_tokens)

colnames(dna_tokens) <- c("dna_full_name", "dna_token")
dna_tokens_uniqueish <- dna_tokens %>% filter(str_detect(dna_token, inc_patt) == FALSE)

dna_tokens_uniqueish %>% regex_inner_join(pharm_comp_names_wfda, by = c("dna_full_name" ="mashup_more_clean"))

dna_patt_plain <- paste0( c(paste0("\\s", dna_tokens_uniqueish$dna_token, "\\s"), paste0("\\b", dna_tokens_uniqueish$dna_token, "\\b")), collapse = "|")
dna_patt_regex <- paste0( c(paste0("\\s", dna_tokens_uniqueish$dna_token, "\\s"), paste0("\\b", dna_tokens_uniqueish$dna_token, "\\b")), collapse = "|")

pharm_comp_names_wfda %>% mutate(dna_token = str_extract(mashup_more_clean, dna_patt_regex)) %>% filter(!is.na(dna_token))
######################################################################################################
######################################################################################################

### still good


## now murky
mashupnames[str_detect(mashupnames$mashup_clean, pattern = "merck|boston|scientific") == TRUE,]
mashupnames

dna_tokens_uniqueish <- dna_tokens_uniqueish %>% mutate(dna_patt = paste0("\\b", dna_token, "\\b"))

regex_inner_join(mashupnames, dna_tokens_uniqueish, by = c("moreclean" = "dna_patt"))
#regex_anti_join(mashupnames, dna_tokens_uniqueish, by = c("moreclean" = "dna_patt"))

fuzzy_inner_join(mashupnames, dna_tokens_uniqueish, by = c("mashup_clean" = "dna_patt"), match_fun = match_fun_stringdist)  %>% filter(dist < 15)

View(mashupnames)
(dna_tokens_uniqueish)
##

test <- regex_inner_join(mashupnames, dna_tokens_uniqueish, by = c("moreclean" = "dna_patt"))

fuzzy_inner_join(mashupnames, dna_tokens_uniqueish, by = c("mashup_clean" = "dna_patt"), match_fun = stringdist)

test %>% select(mashup_clean, moreclean, dna_full_name, dna_token, dna_patt) %>% mutate(dist = match_fun_stringdist(mashup_clean, dna_full_name))

match_fun_stringdist(mashupnames$mashup_clean, dna_tokens_uniqueish$dna_full_name[1])

######

expand.grid(unique(mashupnames$moreclean), unique(dna_tokens_uniqueish$dna_patt)) %>% mutate(
  detect = str_detect(string = as.character(Var1), pattern = as.character(Var2)),
  dist = stringdist::stringdist(Var1, Var2, method = "lcs")) %>%
  filter(detect == TRUE) %>%
  group_by(Var1, Var2) %>% summarise(dist = min(dist))

dna_tokens_uniqueish$dna_full_name
unnest_tokens(, token = "ngrams", n = 3)

dna_tokens_uniqueish %>% bigrams(dna_full_name)

expand.grid(unique(mashupnames$moreclean),
            unique(dna_tokens_uniqueish$dna_full_name)) %>%
  mutate(detect = str_detect(string = as.character(Var1), pattern = as.character(Var2)),
         dist = stringdist::stringdist(Var1, Var2, method = "lcs")) %>%
  filter(detect == TRUE) %>%
  group_by(Var1, Var2) %>%
  summarise(dist = min(dist))

quanteda::tokens(mashupnames$moreclean[1])

tibble::tibble(mashupnames$moreclean[1:5], quanteda::tokens_ngrams(quanteda::tokens(mashupnames$moreclean[1:5]), n = 2:4))

<-quanteda::tokens_ngrams(quanteda::tokens(mashupnames$moreclean[1:5]), n = 2, concatenator = " ")








######################################################################################################

# First, need to define match_fun_stringdist
# Code from stringdist_join from https://github.com/dgrtwo/fuzzyjoin
match_fun_stringdist <- function(v1, v2) {

  # Can't pass these parameters in from fuzzy_join because of multiple incompatible match_funs, so I set them here.
  ignore_case = FALSE
  method = "lcs"
  max_dist = 99
  distance_col = "dist"

  if (ignore_case) {
    v1 <- stringr::str_to_lower(v1)
    v2 <- stringr::str_to_lower(v2)
  }

  # shortcut for Levenshtein-like methods: if the difference in
  # string length is greater than the maximum string distance, the
  # edit distance must be at least that large

  # length is much faster to compute than string distance
  if (method %in% c("osa", "lv", "dl")) {
    length_diff <- abs(stringr::str_length(v1) - stringr::str_length(v2))
    include <- length_diff <= max_dist

    dists <- rep(NA, length(v1))

    dists[include] <- stringdist::stringdist(v1[include], v2[include], method = method)
  } else {
    # have to compute them all
    dists <- stringdist::stringdist(v1, v2, method = method)
  }
  ret <- dplyr::data_frame(include = (dists <= max_dist))
  if (!is.null(distance_col)) {
    ret[[distance_col]] <- dists
  }
  ret
}

######################################################################################################
######################################################################################################
######################################################################################################

# What I have
x <- data.frame(idX=1:3, string=c("Motor cycle", "TractorTrailer", "Sailboat"))
y <- data_frame(idY=letters[1:3], seed=c("ractor", "otor cy", "irplan"))

x
y

x[match_fun_stringdist(x, y) == TRUE,] %>% regex_inner_join(y[match_fun_stringdist(x, y) == TRUE,], by = c("string" = "seed"))

expand.grid(x$string, y$seed) %>% mutate(
  detect = str_detect(string = as.character(Var1), pattern = as.character(Var2)),
  dist = stringdist::stringdist(Var1, Var2, method = "lcs")) %>%
  filter(detect == TRUE) %>%
  group_by(Var1, Var2) %>% summarise(dist = min(dist))
stringdist_inner_join(x, y, by = c("string" = "seed"), max_dist = 10, method = "lcs", distance_col = "dist") %>% group_by(string) %>% summarise(dist = min(dist))

# What I want
want <- data.frame(idX=c(1,2), idY=c("b", "a"), string=c("Motorcycle", "TractorTrailer"), seed=c("otorcy", "ractor"))
inner_join(x, y, by=stringr::str_detect(x$string, y$seed))

x %>% regex_inner_join(y, by = c(string = "seed"))

x[stringdist::stringdist(x, y, method = "lcs")]

x %>% mutate()


x <- data_frame(a = c("france", "franc"), b = c("arras", "dijon"))
y <- data_frame(a = c("franc", "france"), b = c("arvars", "dijjon"))
fuzzy_join(x, y,  match_fun = str_dect)
str_detect(string = "france", pattern = "franc")

######################################################################################################
######################################################################################################
######################################################################################################



fda_nomatch[5:10,]

grep(x = )

grep '\(^\| \)\([A-Za-z]\)\2\2\($\| \)' file

grepl(x = "epi", pattern = mashup_lower_patt)
mashupnames[grepl(x = mashupnames$mashup_clean, pattern = fda_patt) == TRUE, ]

mashupnames$text <- mashupnames$mashup_clean
thing <- tibble(mashupnames, corpus::text_tokens(mashupnames))
tidyr::unnest(thing)

thing <- mashupnames %>% corpus::text_tokens() %>% tibble(mashupnames$text)
colnames(thing) <- c("mashup_tokens", "mashup_lower")
thing <- tidyr::unnest(thing)
thing <- thing %>% filter(str_detect(mashup_tokens, "inc|corp|\\bde\\b|\\bco\\b|llc|ltd|\\scom\\s|int|and|&|\\bna\\b|\\bNA\\b") == FALSE)
thing <- thing %>% group_by(mashup_lower) %>% mutate(mashup_lower2 = paste0(mashup_tokens, collapse = " ")) %>% group_by(mashup_lower, mashup_lower2) %>% summarise(n = n())

mashup_patt <- paste(thing$mashup_lower2, collapse = "|")
mashup_patt2 <- paste("^", thing$mashup_lower2, "$", collapse = "|")
View(thing)



fda_nomatch %>% mutate(match = str_extract(string = lower, pattern = mashup_patt),
                       match2 = ifelse(!is.na(match), str_extract(string = match, pattern = ))) %>% filter(!is.na(match))
thing %>% filter(str_extract())

grepl(x = fda_df$lower, pattern =  mashup_patt)

View(mashup_lower_patt)
str_detect(chars, pattern = value)

mashupnames$mashup_orig




head(mashupnames)

fda_df <- as.data.frame(fda)
colnames(fda_df) <- c("FDA_Company_Name")

head(fda_df)
head(mashupnames)
mashupnames <- mashupnames %>% mutate(nopunct = str_remove_all(mashup, "[.|,]"),
                                      lower = tolower(nopunct))
fda_df <- fda_df %>% mutate(nopunct = str_remove_all(FDA_Company_Name, "[.|,]"),
                            lower = tolower(nopunct))

mashupnames$nopunct <- str_remove_all(mashupnames$nopunct, "[\\\\|/]")

#(str_remove_all(str_trim(mashupnames$nopunct), "[\\\\|/]"))

mashupnames$lower <- str_remove_all(mashupnames$lower, "[\\\\|/]")

mashup_nopunct_patt <- paste0(unique(mashupnames$nopunct), collapse = "|")
mashup_lower_patt <- paste0(unique(mashupnames$lower), collapse = "|")
test <- fda_df %>% mutate(mashup_np_match = str_extract_all(nopunct, patt = mashup_nopunct_patt),
                          mashup_low_match = str_extract_all(lower, patt = mashup_lower_patt)) %>% filter(mashup_np_match != " ")

nchar(test$mashup_np_match)
unique(test$mashup_np_match)
head(test$mashup_np_match)
test$mashup_low_match <- unlist(lapply(test$mashup_low_match, function(x) if(identical(x, character(0))) NA_character_ else x))

fda[str_detect(fda, "^Z") == TRUE]
tail(test, 20)
mashupnames %>% select(mashup) %>%filter(str_detect(mashup, "[Gg][Ee][Nn][Ee][Nn]"))


# Method 1: using the native R adist
# source1.devices<-read.csv('[path_to_your_source1.csv]')
# source2.devices<-read.csv('[path_to_your_source2.csv]')
# To make sure we are dealing with charts
# source1.devices$name<-as.character(source1.devices$name)
# source2.devices$name<-as.character(source2.devices$name)

# It creates a matrix with the Standard Levenshtein distance between the name fields of both sources
companyname_matrix_stringdist <-stringdist(fda_df$FDA_Company_Name,mashupnames$mashup, method = "lv", useBytes = FALSE)  #, partial = TRUE, ignore.case = TRUE)
companyname_matrix <- vector()
for (i in 1:length(fda_df$FDA_Company_Name)) {
  fda_df$FDA_Company_Name[i]
  < - stringdist(fda_df$FDA_Company_Name,mashupnames$mashup, method = "lv", useBytes = FALSE)
}

# We now take the pairs with the minimum distance
min.name <- apply(companyname_matrix, 1, min)
match.s1.s2<-NULL
for(i in 1:nrow(companyname_matrix)) {
  s2.i<-match(min.name[i],companyname_matrix[i,])
  s1.i<-i
  match.s1.s2<-rbind(data.frame(s2.i=s2.i,s1.i=s1.i,s2name=mashupnames[s2.i,]$mashup, s1name=fda_df[s1.i,]$FDA_Company_Name, adist=min.name[i]),match.s1.s2)
}
# and we then can have a look at the results
head(match.s1.s2)

test$text <- test$lower
test2 <- test %>% corpus::text_tokens() %>% tibble(test$text)  # %>% str_remove_all(pattern = "inc|co|llc|ltd|co") mutate(mashup_match_ain = stringdist::ain(lower, mashupnames$lower))
test2 <- tidyr::unnest(test2)
colnames(test2) <- c("fda_lower", "fda_lower_token")
test2 %>% filter(str_detect(fda_lower_token, pattern = "inc|co|llc|ltd|co|int|and|&") == FALSE) %>% group_by(fda_lower) %>% mutate(fda_lower2 = paste0(fda_lower_token, collapse = " "))

test$lower %>% str_remove_all(pattern = "inc|co|llc|ltd|co|int|and|&") %>% str_trim()

pharm_comp_names_wfda %>% filter(str_detect(SIC_Company_Name, "^GEN"))

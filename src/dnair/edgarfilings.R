txtfile <- "data/business_innovation/original/edgar_filings/Edgar filings/1001316_10-K_2013/1001316_10-K_2013-03-21.txt"
edgar <- readr::read_tsv(txtfile)
edgar1 <- as.character(edgar1)

edgar6 <- grep(".*<DOCUMENT>\\s<TYPE>XML\\s*|</DOCUMENT>*", edgar1, value = TRUE)
edgar6 <- stringr::str_extract(edgar1, ".*<DOCUMENT>\\s<TYPE>XML\\s*|</DOCUMENT>*", simplify = FALSE)
edgar6 <- stringr::str_extract(edgar1, "<DOCUMENT>\n<TYPE>.<//DOCUMENT>")
write(edgar6, "data/business_innovation/working/dndummy.csv")
write.table(edgar6, file = "data/business_innovation/working/dndummy.txt", sep = "\t",
            row.names = FALSE)
#edgar6 <- gsub(".*<DOCUMENT>\\s*|</DOCUMENT>*", "", edgar1)

#edgar6 <- str_extract(string = edgar1, pattern = regex("(?<=<DOCUMENT>).*(?=</DOCUMENT>)"))

edgar3 <- xml2::read_xml(txtfile) # couldn't get this to work, but this is how it needs to work
edgar5 <- xml2::read_html(txtfile)

edgar7 <- xml2::read_html(edgar1)






url <- "https://www.sec.gov/Archives/edgar/data/318154/000095012311018800/v57113e10vk.htm"
edgar4 <- xml2::read_html(url)

library(xml2)
library(rvest)

div <- xml_find_all(edgar4, ".//div")
p <- xml_find_all(edgar4, ".//p")
div_text <- html_text(div)
div_text_clean <- stringr::str_squish(div_text)

#take SEC filing texts that have at least 20 characters
div_text_clean_20plus <- subset(div_text_clean, nchar(div_text_clean) >= 20)


## tokenizing words manually
library(hunspell)
library(data.table)
library(dplyr)

row <- div_text_clean_20plus[200]
row_words <- unlist(stringr::str_split(gsub('[[:punct:] ]+',' ', row), " ", simplify = FALSE))
row_eng <- hunspell_check(row_words, dict = dictionary("en_US"))
row_caps <- grepl("^[[:upper:]]", row_words)
row_FDA <- grepl("FDA", row_words)
word_innov<- c("launch", "new product")
row_launch <- grepl(paste(word_innov, collapse = "|"), row_words)
row_launch2 <- grepl(paste(word_innov, collapse = "|"), tolower(div_text_clean_20plus[1:50]))

row_launch2 <- grepl(paste(word_innov, collapse = "|"), tolower(div_text_clean_20plus[48]))


which(grepl(paste(word_innov, collapse = "|"), tolower(div_text_clean_20plus[1:1000])) == TRUE)

words <- unlist(stringr::str_split(gsub('[[:punct:] ]+',' ', div_text_clean_20plus[48]), " ", simplify = FALSE))
eng <- hunspell_check(words, dict = dictionary("en_US"))
caps <- grepl("^[[:upper:]]", words)

example2 <- tibble::tibble("Word" = words,
                           "Capitals" = caps,
                           "English" = eng) %>% data.table::as.data.table()

View(example2)


#row_position <- row_words %>% mutate("Position" = row_number())
#library(stringdist)
library(qdap)
qdap::word_proximity("launch", div_text_clean_20plus[200])
word_fda <- c("FDA", "approval")
stringdist(word_innov, row_words, q = 1)

# Raghav input on additional terms/proximities?
# proximity - within 15 words

example2 <- tibble::tibble("Word" = row_words,
                  "Capitals" = row_caps,
                  "English" = row_eng,
                  "Launch" = row_launch,
                  "Position" = row_position) %>%
  as.data.table()

View(example2)
test <- example2 %>% filter(English == FALSE) %>% arrange(desc(Capitals))
test[1:10,]
#test[7,1]

###PRESIDENTIAL EXAMPLE
wrds <- word_list(pres_debates2012$dialogue,
                  stopwords = c("it's", "that's", Top200Words))
wrds2 <- tolower(sort(wrds$rfswl[[1]][, 1]))
(x <- with(pres_debates2012, word_proximity(dialogue, wrds2)))
plot(x)
plot(weight(x))
plot(weight(x, "rev_scale_log"))

(x2 <- with(pres_debates2012, word_proximity(dialogue, wrds2, person)))

## The spaces around `terms` are important
(x3 <- with(DATA, word_proximity(state, spaste(qcv(the, i)))))
(x4 <- with(DATA, word_proximity(state, qcv(the, i))))


#reformat the object?
test <- as.data.frame(head(div_text_clean_20plus))
test <- as.data.frame((div_text_clean_20plus[200]))
colnames(test) <- c("Words")
test <- test %>% dplyr::mutate(dummy = 1)
test$Words <- as.character(test$Words)
check_text(test$Words)
qdaptest <- sentSplit(test, "Words")

wrds <- word_list(qdaptest$Words)
wrds2 <- tolower(sort(wrds$fwl[[1]][,1]))
(x <- with(qdaptest, word_proximity(Words, wrds2)))




####JUNK

example <- as.data.table(example) # %>% filter(English == FALSE)
View(example)

# words_example <- as.data.frame(unlist(word_ex2))
# colnames(words_example) <- c("Word")
# words_example %>%
#   mutate("English" = hunspell_check(as.character(words_example$Word), dict = dictionary("en_US"))) %>%
#   as.data.table()
#
# div_text_clean_20plus[100]
#
# word_ex2 <- stringr::str_split(div_text_clean_20plus[100], " ", simplify = FALSE)
# word_ex2_caps <- grepl("^[[:upper:]]", unlist(word_ex2))
# word_ex2_eng <- hunspell_check(unlist(word_ex2), dict = dictionary("en_US"))
#
# example <- tibble::tibble("Word" = unlist(word_ex2),
#                           "Capitals" = word_ex2_caps,
#                           "English" = word_ex2_eng)

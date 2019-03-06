txtfile <- "data/business_innovation/original/edgar_filings/Edgar filings/1001316_10-K_2013/1001316_10-K_2013-03-21.txt"
#edgar1 <- readr::read_tsv(file)
url <- "https://www.sec.gov/Archives/edgar/data/318154/000095012311018800/v57113e10vk.htm"
edgar4 <- xml2::read_html(url)

edgar3 <- xml2::read_xml(txtfile)

edgar4
class(edgar4)
library(xml2)
library(rvest)
xml_name(edgar4) #html
children <- xml_children(edgar4)

#baz <- xml_find_all(x, ".//baz")
div <- xml_find_all(edgar4, ".//div")
div_text <- html_text(div)
div_text_clean <- stringr::str_squish(div_text)

div_text_clean_20plus <- subset(div_text_clean, nchar(div_text_clean) >= 20)


## tokenizing words manually
library(hunspell)
div_text_clean_20plus[100]

word_ex2 <- stringr::str_split(div_text_clean_20plus[100], " ", simplify = FALSE)
word_ex2_caps <- grepl("^[[:upper:]]", unlist(word_ex2))
word_ex2_eng <- hunspell_check(unlist(word_ex2), dict = dictionary("en_US"))

example <- tibble::tibble("Word" = unlist(word_ex2),
       "Capitals" = word_ex2_caps,
       "English" = word_ex2_eng)

library(data.table)
library(dplyr)
example <- as.data.table(example) # %>% filter(English == FALSE)
View(example)

# words_example <- as.data.frame(unlist(word_ex2))
# colnames(words_example) <- c("Word")
# words_example %>%
#   mutate("English" = hunspell_check(as.character(words_example$Word), dict = dictionary("en_US"))) %>%
#   as.data.table()

## another example
row <- div_text_clean_20plus[200]
row_words <- unlist(stringr::str_split(gsub('[[:punct:] ]+',' ', row), " ", simplify = FALSE))

row_eng <- hunspell_check(row_words, dict = dictionary("en_US"))
row_caps <- grepl("^[[:upper:]]", row_words)
row_FDA <- grepl("FDA", row_words)
word_innov<- c("launch", "new", "product", "FDA", "approval", "innovation", "innovative")
row_launch <- grepl(paste(word_innov, collapse = "|"), row_words)
#row_position <- row_words %>% mutate("Position" = row_number())
library(stringdist)
library(qdap)

library(devtools)
install_github("trinker/qdapDictionaries")
install_github("trinker/qdapRegex")
install_github("trinker/qdapTools")
install_github("trinker/qdap")
install.packages("qdap", INSTALL_opts = "--no-multiarch")
library()
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


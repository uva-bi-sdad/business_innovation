txtfile <- "data/business_innovation/original/edgar_filings/Edgar filings/1001316_10-K_2013/1001316_10-K_2013-03-21.txt"
#edgar1 <- readr::read_tsv(file)
url <- "https://www.sec.gov/Archives/edgar/data/318154/000095012311018800/v57113e10vk.htm"
edgar4 <- xml2::read_html(url)

edgar3 <- xml2::read_xml(txtfile)

edgar4
class(edgar4)
library(xml2)
xml_name(edgar4) #html
children <- xml_children(edgar4)

baz <- xml_find_all(x, ".//baz")
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

example <- tibble("Word" = unlist(word_ex2),
       "Capitals" = word_ex2_caps,
       "English" = word_ex2_eng)

example %>% filter(English == FALSE)

words_example <- as.data.frame(unlist(word_ex2))
colnames(words_example) <- c("Word")
words_example %>% mutate("English" = hunspell_check(words_example$Word, dict = dictionary("en_US")))

## another example
row <- div_text_clean_20plus[200]
row_words <- unlist(stringr::str_split(gsub('[[:punct:] ]+',' ', row), " ", simplify = FALSE))

row_eng <- hunspell_check(row_words, dict = dictionary("en_US"))
row_caps <- grepl("^[[:upper:]]", row_words)

example2 <- tibble("Word" = row_words,
                  "Capitals" = row_caps,
                  "English" = row_eng)

test <- example2 %>% filter(English == FALSE) %>% arrange(desc(Capitals))
test[7,1]

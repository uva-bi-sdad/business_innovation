library(igraph)
library(dplyr)
library(tidytext)
library(janeaustenr)
library(quanteda)

c222 <- read.csv("./data/working/DNA_Aggregated/c22_articles_2015.csv", stringsAsFactors = FALSE)
c22 <- data.frame(c222$subject_codes)


c22 <- c22 %>%
  dt_mutate(
    c222.subject_codes = str_replace_all(c222.subject_codes, ","," ") %>%
      map(unlist),
    c222.subject_codes = map(.x = c222.subject_codes, ~ifelse(is.null(.x), character(0), .x))
    #subject_codes_split = map(.x = subject_codes_split, ~ifelse(is.null(.x), character(0), .x))
  )


text <- c22$c222.subject_codes
texy <- unlist(text, recursive = TRUE, use.names = TRUE)
head(texy)
text_df <- tibble(c22$ID, text = texy)

class(text)
class(texy)

c22$ID <- seq.int(nrow(c22))
class(text_df)

text_df %>%
  unnest_tokens(word, texy, token = "ngrams", n = 2)


#c_22 <- as.tibble(c22)
#c22$c222.subject_codes = character()

bigram_df <- c_22 %>%
  unnest_tokens(output = bigram, input = c_22, token = "ngrams", n = 2, collapse = TRUE)

c22$c222.subject_codes = as.character(c22$c222.subject_codes)


str(c22)

library(dplyr)
library(tidyr)
library(tidytext)
#library(ggplot2)
library(igraph)
library(ggraph)

bigrams(c22$c222.subject_codes, window = 1, concatenator = " ", include.unigrams = FALSE,
        ignoredFeatures = NULL, skipGrams = FALSE)


count_bigrams <- function(c22) {
  c22 <- unnest_tokens(bigram, text, token = "ngrams", n = 2)
  c22 <- separate(bigram, c("word1", "word2"), sep = " ")
  c22 <- count(word1, word2, sort = TRUE)
}

visualize_bigrams <- function(bigrams) {
  set.seed(2016)
  a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

  bigrams %>%
    graph_from_data_frame() %>%
    ggraph(layout = "fr") +
    geom_edge_link(aes(edge_alpha = n), show.legend = FALSE, arrow = a) +
    geom_node_point(color = "lightblue", size = 5) +
    geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
    theme_void()
}

c22_bigrams <- c22 %>%
  count_bigrams()


bigram_counts









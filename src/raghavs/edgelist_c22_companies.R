library(igraph)

c222 <- read.csv("./data/working/DNA_Aggregated/c22_articles_2015.csv", colClasses = "character")
c22 <- data.frame(c222$subject_codes)


c22 <- c22 %>% 
  dt_mutate(
    subject_codes_split = str_replace_all(c222.subject_codes, ","," ") %>%
      map(unlist),
    c222.subject_codes = map(.x = c222.subject_codes, ~ifelse(is.null(.x), character(0), .x)),
    subject_codes_split = map(.x = subject_codes_split, ~ifelse(is.null(.x), character(0), .x))
  )

c22 <- data.frame(c22$subject_codes_split)
c22
bigram_counts


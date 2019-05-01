
file_names <- list.files("data/business_innovation/working/sec/secwordlists/", full.names = TRUE)

for (i in seq_along(file_names)) {
  read_in <- read.csv(file_names[i])

  if (exists("read_out") == FALSE)
    read_out <- read_in
  else
    read_out <- rbindlist(list(read_out, read_in))

}

#write_csv(read_out, "data/business_innovation/working/sec/all_secwordlists.csv")

allwords <- read_csv("data/business_innovation/working/sec/all_secwordlists.csv")

topenglishwords <- allwords %>%
  mutate(lowword = str_to_lower(Words)) %>%
  group_by(lowword) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  subset(!(lowword %in% stopwords_en))  %>%
  filter(nchar(lowword) >= 3)

View(topenglishwords)

topenglishwords[1:15,]

library(readr)
CMDA_FALL2019_headlines_labeled <- read_csv("git/business_innovation/data/working/DNA_Aggregated/CMDA_FALL2019_headlines_labeled.csv")

dna_2015 <- readRDS("~/git/business_innovation/data/working/DNA_Aggregated/dna_2015.RDS")
head(dna_2015)
colnames(dna_2015)
library(dplyr)

dna_2015 <- dna_2015 %>% select(an, title, body, company_codes)

class(dna_2015$an)
class(CMDA_FALL2019_headlines_labeled$document)

CMDA_labeled_full_articles <- CMDA_FALL2019_headlines_labeled %>% left_join(dna_2015, by = c("document"= "an", "title"))
View(CMDA_labeled_full_articles)

colnames(CMDA_labeled_full_articles) <- c("id", "title", "labeller", "innovYN", "c22", "FDAYN", "body", "company_codes")
write_csv(CMDA_labeled_full_articles, "~/git/business_innovation/data/working/DNA_Aggregated/CMDA_FALL2019_hlwbody_hlbasedlabel.csv")

library(dplyr)
library(stringr)

## Step 1 - remove NA dates - 4344 to 4173 (diff of 171)
## Step 2 - remove NA texts - 4173 to 3716 (diff of 457)

proqpharm1 <- read.csv("data/business_innovation/working/parsedProquestData/naics325411.csv")
proqpharm2 <- read.csv("data/business_innovation/working/parsedProquestData/naics325412.csv")
proqpharm3 <- read.csv("data/business_innovation/working/parsedProquestData/naics325413.csv")
proqpharm4 <- read.csv("data/business_innovation/working/parsedProquestData/naics325414.csv")

proqpharm <- rbind(proqpharm1, proqpharm2, proqpharm3, proqpharm4)
proqpharm_orig <- rbind(proqpharm1, proqpharm2, proqpharm3, proqpharm4)

proqpharm$Publication.date <- lubridate::ymd(proqpharm$Publication.date)
proqpharm <- proqpharm %>% filter(!is.na(Publication.date)) %>% filter(!is.na(Full.Text))

colnames(proqpharm)
head(unique(proqpharm$Subject, 10))
head(unique(proqpharm$Publication.subject, 10))
head(unique(proqpharm$Source.type, 10))
head(unique(proqpharm$Document.type, 10))

factiva_detect <- c(
  "innovat", "novel",
  "new product", "new drug", "new device", "new offering",
  "announces new", "announced new", "announce launch", "announced new", "announced a new", "announced (the) launch", # double check the regex
  "launch", "invent",  "discover", "develop", "introduc" #"releas",
  )

factiva_detective <- c(
  "market", "marketplace", "commercializ", "pric",
  "sell", "deal", "retail", "public", "consumer",
  "business", "strateg", "enterprise", "contract", "claim",
  "produc", "distribut", "acqui"
)

factiva_detect <- paste0("\\s", factiva_detect, collapse = "|")
factiva_detective <- paste0("\\s", factiva_detective, collapse = "|")

#proqpharm$Full.Text[2]
#str_extract_all(pattern = factiva_detect, string = proqpharm$Full.Text[1])
#str_extract_all(pattern = factiva_detective, string = proqpharm$Full.Text[1])
#str_detect(pattern = factiva_detective, string = proqpharm$Full.Text[1])

proqpharm <- proqpharm %>% mutate(newprod = str_detect(string = Full.Text, pattern = factiva_detect),
                     busind = str_detect(string = Full.Text, pattern = factiva_detective))
proqpharm[1:5, c(1:8, 10:11)]
proqpharm[1:5, 9]

proqpharm %>% group_by(newprod, busind) %>% summarise(n = n())

proqpharm_sub <- proqpharm %>% filter(newprod == TRUE & busind == TRUE)

proqpharm_sub$Full.Text[1]

head(unique(proqpharm_sub$Subject, 10))
head(unique(proqpharm_sub$Publication.subject, 10))
head(unique(proqpharm_sub$Source.type, 10))
head(unique(proqpharm_sub$Document.type, 10))

length(unique(proqpharm_sub$Subject))
length(unique(proqpharm_sub$Publication.subject))
length(unique(proqpharm_sub$Source.type))
length(unique(proqpharm_sub$Document.type))

pubsubs <- proqpharm_sub %>% select(Publication.subject) %>% unique()
subjects <- proqpharm_sub %>% select(Subject) %>% unique()
titles <- proqpharm_sub %>% select(Article.Title) %>% unique()

pubsubjnewprod <- pubsubs %>% mutate(pubsubs = str_detect(str_trim(pubsubs$Publication.subject), pattern = factiva_detect)) %>% filter(pubsubs == TRUE)
subjnewprod <- subjects %>% mutate(subjs = str_detect(str_trim(subjects$Subject), pattern = factiva_detect)) %>% filter(subjs == TRUE)
titlenewprod <- titles %>% mutate(titles = str_detect(str_trim(titles$Article.Title), pattern = factiva_detect)) %>% filter(titles == TRUE)


proqpharm_sub <- proqpharm_sub %>%
  left_join(subjnewprod, by = "Subject") %>%
  left_join(titlenewprod, by = "Article.Title")

proqpharm_sub[1:10,1:8]
colnames(proqpharm_sub)
proqpharm_sub$Article.Title <- str_trim(proqpharm_sub$Article.Title)
proqpharm_sub$Article.Title <- str_trim(proqpharm_sub$Company[1:8])
typeof(str_trim(proqpharm_sub$Company[1:8]))

sample <- as.data.frame(proqpharm_sub[1:3, 3])
colnames(sample) <- c("Company")

sample %>%  mutate(companies = strsplit(as.character(Company), ";")) %>% tidyr::unnest(companies) %>% strsplit(companies, ":") #%>% reshape2::melt(Company)

nrow(proqpharm_orig)
nrow(proqpharm_sub) #got to 2222 by ensuring text had both innovation words and business words
nrow(subjnewprod) #got to 155 by ensuring subject had innovation words
# still not convinced this is right set of articles

as.vector(unique(proqpharm$Document.type))

'%ni%' <- Negate('%in%')

proqpharm_sub %>% filter(str_detect(Document.type, pattern = "Book Review-Favorable|Book Review-Mixed|Interview") == FALSE)

proqpharm %>% group_by(Document.type) %>% summarise(n = n()) %>% View()

as.data.frame(proqpharm_sub, row.names = rownames(proqpharm_sub))
proqpharm_sub <- proqpharm_sub %>% mutate(row = row.names(proqpharm_sub))

sample2 <- proqpharm_sub %>% filter(subjs == "TRUE" | titles == "TRUE")



sample3 <- tibble::tibble(sample2$row, sample2$Full.Text)

saveRDS(sample4, "data/working/dn_sampleproquest_testsurvey2.RDS")

#test <- split.data.frame(sample3, )

sample4 <- split(sample3, rep(1:ceiling(nrow(sample3)/10), each=10, length.out=nrow(sample3)))

write.table(tibble::tibble(sample2$row, sample2$Full.Text), file = "data/working/dn_sample2_survey.txt", sep = "\t",
            row.names = FALSE, col.names = NA)



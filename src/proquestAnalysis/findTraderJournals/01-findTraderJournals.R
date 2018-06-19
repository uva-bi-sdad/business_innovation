# this script finds all the unique trade journals in the ProQuest database

library(dplyr)
pharmaProQuestData <- read.csv('./data/business_innovation/working/parsedProquestData/naics325412.csv')

# extract all the unique source name in the source type

tradeJournals <- filter(pharmaProQuestData, Source.type == 'Trade Journals') %>%
  select(Source.type, Publication.title)

uniqueTradeJournals <- unique(tradeJournals)

write.csv(uniqueTradeJournals, './data/business_innovation/working/ProQuestTradeJournals.csv',row.names = FALSE)


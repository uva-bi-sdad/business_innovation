## creating bigrams
library(data.table)
library(dplyr)
library(tm)
library(stringr)
library(tree)
library(party)
library(reshape2)
library(ggplot2)
library(rpart)
library(tidytext)
library(stringr)

docResultsFull = fread("./data/business_innovation/original/documenterResults/document_ratings_june13.csv")


docCorpus = Corpus(VectorSource(docResultsFull[rated == TRUE, html])) %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(removeWords, c("the", "and", stopwords("english"))) %>%
  tm_map(stripWhitespace)

# extract the content into dataframe
dfCorpus <- sapply(docCorpus, '[[', 1) %>%
  purrr::map_df(data.frame, stringsAsFactors = FALSE)
names(dfCorpus) <- 'corpus'
testthat::expect_equal(nrow(dfCorpus), length(docCorpus))

#extracting bigrams for each entry in the dfCorpus

docBigram <- dfCorpus %>%
  unnest_tokens(bigram, corpus, token = "ngrams", n = 2)

# counting and filtering the bigrams
docBigram %>%
  count(bigram, sort = TRUE)

# count the number of times fda approval, new products, and product launch appear
fda <- count(filter(docBigram, grepl("fda approval", bigram))) #31 times
np <- count(filter(docBigram, grepl("new product", bigram))) # 32 times
pl <- count(filter(docBigram, grepl("product launch", bigram))) # 6 times

# create a histogram that plots the number of times these bigrams show up on the aggregate level
docBigram.freq <- table(docBigram)
barplot(sort(docBigram.freq, decreasing = TRUE))
bigramTable <- data.frame(docBigram.freq) %>%
  arrange(-Freq)
bigramTable <- bigramTable[1:10,]

ggplot(data = bigramTable) +
  geom_bar(mapping = aes(x =forcats::fct_reorder(docBigram, -Freq),
                         fill = Freq,
                         y = Freq), stat = 'identity') +
  theme(axis.text.x  = element_text(angle=45, vjust=1, hjust=1)) + theme(axis.text = element_text(size = 14)) +
  scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 20)) +
  labs(title = "Bigram Plot", x = "Bigrams", y = "Frequency", fill = 'frequency')

## feature creations--str_detect for dfCorpus
NP <- vector(mode="numeric", length=nrow(dfCorpus))
for(i in 1:nrow(dfCorpus)) {
  NP[i] <- str_detect(dfCorpus[i,], 'new product')
}

# NP <- data.frame(NP)
## feature creations--fda approval
FDA <- vector(mode="numeric", length=nrow(dfCorpus))
for(i in 1:nrow(dfCorpus)) {
  FDA[i] <- str_detect(dfCorpus[i,], 'fda approval')
}

# FDA <- data.frame(FDA)
## feature creations--product launches
PL <- vector(mode="numeric", length=nrow(dfCorpus))
for(i in 1:nrow(dfCorpus)) {
  PL[i] <- str_detect(dfCorpus[i,], 'product launch')
}

# PL <- data.frame(PL)

# bigram per document/bigram document matrix

## attaching the new features to the original dataframe

dfCorpus$NP <- NP
dfCorpus$FDA <- FDA
dfCorpus$PL <- PL
# bigrams to evaluate
tdResponseMatrix <- fread("./data/business_innovation/working/DocumenterModelData/termDocumentResponseMatrix.csv")
# combining td response matrix with new features
bigramAndTd <- data.frame(tdResponseMatrix, dfCorpus[,c("NP", "FDA", "PL")])

# svm on new features
library(e1071)
svm.model <- svm(as.factor(y) ~ ., data = bigramAndTd, cost = 1000, gamma = 0.0001)
svm.pred <- predict(svm.model, bigramAndTd)
table(svm.pred, bigramAndTd$y)

#cross validate that shit

## tree
docTree = tree(as.factor(y) ~ ., method = 'deviance', data = bigramAndTd)
docTree
plot(docTree)
text(docTree)
table(predict(docTree, bigramAndTd, type = 'class'), bigramAndTd$y)

# Pruning
pruneDocTree = prune.tree(docTree, method = 'deviance')
# CV
cvDocTree = cv.tree(docTree, method = 'deviance')


pruneCvPlot = data.frame(size = pruneDocTree$size, pruneMisclass = pruneDocTree$dev, cvMisclass = cvDocTree$dev) %>%
  melt(id.vars = "size") %>%
  `colnames<-`(c("size", "method", "Deviance"))

ggplot(data = pruneCvPlot) +
  geom_line(aes(x = size, y = Deviance, col = method)) +
  geom_point(aes(x = size, y = Deviance, col = method)) + theme(text = element_text(size = 22))

finalTree = prune.tree(docTree, best = 5, method = 'misclass')
finalTree; plot(finalTree); text(finalTree)
table(prediction = predict(finalTree, data.frame(predMat), type = 'class'), actual = bigramAndTd$y)
summary(finalTree)

library(data.table)
library(dplyr)
library(tm)
library(stringr)
library(tree)
library(party)
library(reshape2)
library(ggplot2)
library(rpart)
docResultsFull = fread("./data/business_innovation/original/documenterResults/document_ratings_june13.csv")

# common words in product launch articles, 'new product' in launch articles
# count by article type, where does the product appear, words common to launch/approval articles, is company in company column

docCorpus = Corpus(VectorSource(docResultsFull[rated == TRUE, html])) %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(removeWords, c("the", "and", stopwords("english"))) %>%
  tm_map(stripWhitespace)

# TFIDF stuff
# docTfidf = DocumentTermMatrix(docCorpus, control = list(weighting = weightTfIdf))
# docTfidf = removeSparseTerms(docTfidf, 0.95)
# docTfidf

docDTM = DocumentTermMatrix(docCorpus)
#docDTM = docTfidf
str(docDTM)
aboutProduct = docResultsFull[rated == TRUE, articleTopic] %in% c('fdaApproval', 'launch')


docMat = data.table(doc = docDTM$i, word = docDTM$dimnames$Terms[docDTM$j], count = docDTM$v) %>%
  merge(data.table(doc = seq_along(aboutProduct), aboutProduct = aboutProduct))

# This gives how many words appear a given number of times in total
wordTable = docMat[,.(count = sum(count)), by = c('word')] %>% arrange(-count) %>% data.table
table(wordTable$count)
#18722 - sum(table(wordTable$count)[1:20])

# We require a word to appear at least this many times to be included in the analysis
minWordCountThreshold = 40

survivorWords = wordTable[count >= minWordCountThreshold, word]

# Now we look at words broken out by topic

reducedWordCount = docMat[word %in% survivorWords]
reducedWordCount = data.table(reducedWordCount, filterWordIndex = match(reducedWordCount$word, unique(reducedWordCount$word)))

#
#reducedWordCount = data.table(docMat, filterWordIndex = match(docMat$word, unique(docMat$word)))
#

# Turn into a long format matrix

predMat = matrix(0, nrow = length(docCorpus), ncol = length(unique(reducedWordCount$word)))
for(i in 1:nrow(reducedWordCount)) predMat[reducedWordCount$doc[i], reducedWordCount$filterWordIndex[i]] = reducedWordCount$count[i]
rownames(predMat) = paste0('doc', 1:nrow(predMat))
colnames(predMat) = unique(reducedWordCount$word)

predMat[1:5, 1:5]
# For combining fda approval with product launches
y = as.factor(aboutProduct)

# Three class problem
yThree = docResultsFull[rated == TRUE, articleTopic]
yThree[yThree == ""] = "neither"
yThree = as.factor(yThree)

fwrite(data.table(y = yThree, predMat), "./data/business_innovation/working/DocumenterModelData/termDocumentResponseMatrix.csv")

# fit some tree

docTree = tree(y ~ ., control = tree.control(length(y), minsize = 20), method = 'deviance', data = data.frame(y, predMat))
docTree
plot(docTree)
text(docTree)
table(predict(docTree, data.frame(predMat), type = 'class'), y)

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

finalTree = prune.tree(docTree, best = 6, method = 'misclass')
finalTree; plot(finalTree); text(finalTree)
table(prediction = predict(finalTree, data.frame(predMat), type = 'class'), actual = y)
summary(finalTree)

#
# Rpart
#

rpartDocTree = rpart(y ~ predMat, control = rpart.control(minsplit = 40))
rpartDocTree
plot(rpartDocTree)
text(rpartDocTree)


#
# three class problem
#


# fit some tree

docTree = tree(yThree ~ predMat, control = tree.control(length(yThree), minsize = 20), method = 'misclass')
docTree
plot(docTree)
text(docTree)
table(prediction = predict(docTree, data.frame(predMat), type = 'class'), actual = yThree)

# Pruning
pruneDocTree = prune.tree(docTree, method = 'misclass')
# CV
cvDocTree = cv.tree(docTree, method = 'misclass')


pruneCvPlot = data.frame(size = pruneDocTree$size, pruneMisclass = pruneDocTree$dev, cvMisclass = cvDocTree$dev) %>%
  melt(id.vars = "size") %>%
  `colnames<-`(c("size", "method", "Misclassifications"))

ggplot(data = pruneCvPlot) +
  geom_line(aes(x = size, y = Misclassifications, col = method)) +
  geom_point(aes(x = size, y = Misclassifications, col = method))

finalTree = prune.tree(docTree, best = 5, method = 'misclass')
finalTree; plot(finalTree); text(finalTree)
table(predict(finalTree, data.frame(predMat), type = 'class'), yThree)
summary(finalTree)

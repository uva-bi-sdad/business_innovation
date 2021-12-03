# Emily Kurtz
# 10/2/21
# some cleaning function ideas
# use regex to remove tags
# remove stop words
### remove stop words and generate new stop words with some test txt files?
### perhaps store state, zip code as features of the txt file? could cross check that with a standard list
### are there any other features that we might just want to store and then remove any reference of after that?
# stem
# foreign languages?
# search for trademark symbol
# copyright, trademark, launch
# company product pair list
# clean, produce counts, sentences, extract words
# look at five years, 2016-2021


removeTags <- function(txt){
  #take in a txt file that is scraped from edgar, output a txt file that has the html tags removed
  #the html tags don't include any useful innovation information
  #i may want to confirm above claim with neil and aritra
  #can do this work using regex

  #creates a new column
  names(txt) <- "Original"
  txt$NoTags <- gsub("<.*>","",as.character(txt$Original))
  txt
}




#TESTS
# Removing tags seems pretty crucial in the txt file found here:
# https://www.sec.gov/Archives/edgar/data/1032562/000095013709002070/0000950137-09-002070.txt
testfile <- read.delim("LandOLakes10K.txt")#,stringsAsFactors = FALSE)
# 403 forbidden if try to read directly from internet using read.table
names(testfile)<-"col"
testfile$col2<-gsub("<.*>","",as.character(testfile$col))
head(testfile,100)
write.table(testfile,file="testRemoveTagsOutput.txt",row.names=FALSE)

#will need to worry about the case when there are multiple <'s, will cause issues as i am doing things now




#stop words
# i plan to get rid of standard stop words, then see what things look like, then get rid of more, and iteratively generate a library of stop words
# maybe i can constantly refine that
# do we maybe want to make a package with this (and maybe all of this in general?)


### FUNCTION TO GENERATE STOP WORD LIST
generateStopWordList <- function(numberOfTXTs,numberOfStopWords,txtFileDirectory)



### FUNCTION TO REMOVE STOP WORDS
removeStopWords <- function(txt,columnNumber,stopWords){
  #want user to specify a txt file with one column and a stop words dictionary as an array
  #can allow to read in whole txt file but make sure specify column that has language wanting to process
  #simply remove all words in txt column that match any in stop words array
  names(txt)[columnNumber] <- "Original"

}




### FUNCTION TO PULL A SET OF SO MANY TESTING TXT FILES FOR GENERATING THE STOP WORDS FROM




### FUNCTION TO DO IT ALL, BRING ALL HELPER FUNCTIONS TOGETHER


#tokenize words AND sentences
# good regex way to separate words into sentences
# clean as txt, raw as csv

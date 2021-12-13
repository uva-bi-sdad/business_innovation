
install.packages("readr")
library(readr)


# Function to get a list of file names of the desired file type ###################################
getFiles <- function( extension = "all", folderName = "") {
  
  
  fileList = list.files()
  # Only look deeper if given a folder name
  if (folderName!="") {
    if (sum(fileList==folderName)==1){
      fileList = list.files(folderName)
    } else if (length(grep(folderName, fileList)==1)){
      fileList = list.files(fileList[grep(folderName, fileList)])
    } else {
      stop("Enter the correct folder name")
    }
  }
  
  # Clear out all folders and files of the wrong extension
  fileList = fileList[grep("\\.W*",fileList)]
  if (extension!="all"){
    extension = paste0("\\.",extension,"$")
    extension = gsub("\\.\\.",".",extension)
    fileList = fileList[grep(extension,fileList)]
  }
  
  return(fileList)
}


# Parsing Keyworkds and sentences they came from#########################################


identifyKeywordSentences <- function(filename, targetExpressions, shrinkFile = FALSE) {
  # Alternate file reading for compressed versions
  if (grepl("\\.gzip$",filename)) {
    rawString = read_file(gzfile(filename))
  } else {
    # Read file as a single string, get rid of HTML tags
    rawString = read_file(filename)
  }
  # Clean up file
  rawString = gsub("<.*?>", " ", rawString)            # get rid of all html tags
  if (shrinkFile) {
    rawString = gsub(" +", " ", rawString)               # remove excess spaces
    rawString = gsub("(\r+\n+ *)+", "\r\n", rawString)   # get rid of excess carriage returns/newlines
    # Add a line to write the smaller file if using this mode
  } else {
    rawString = gsub("\\s+", " ", rawString)               # simplify all spans of whitespace into single spaces
  }
  
  # Split into a list of sentences, using periods followed by whitespace as the identifier
  splitString = strsplit(rawString, "\\.\\s", fixed = FALSE)[[1]]
  
  # Identify all matching sentences in bulk initially.  Merge the supplied regexes the format "(regex1)|(regex2)|...|(regexN)"
  combinedTargets = paste0("(",paste(targetExpressions, collapse = ")|("),")")
  sentenceNumber = grep(combinedTargets, splitString, ignore.case = TRUE)  # identify the ID of each matching sentence
  sentences = splitString[sentenceNumber]                                  # extract only those sentences that matched
  
  # Return a single-row data frame if no matches were found
#  if (length(sentenceNumber)==0) {
#    details = data.frame()
#    details[1,"sentenceNumber"] = NA
#    for (target in targetExpressions) {
#      
#    }
#  }
  
  # Build the data frame with placeholder counts, and define each target column name
  details = data.frame(sentenceNumber = c(NA,sentenceNumber))
  for (target in targetExpressions) {
    details[target] = 0   # Define column name, and set all values to 0 for now
  }
  details["sentenceText"] = c(NA,sentences)
  
  # Add rownames, capping list at the size of the dataframe to avoid errors when no matches were found
  rownames(details) = c(paste0(filename,"-Totals"),paste0(filename,"-S",c(1:length(sentenceNumber))))[1:nrow(details)]
  
  # Escape early if there were not matches, errors happen if we go further
  if (length(sentenceNumber)==0) {return(details)}
  
  # Count the number of times each target expression appears in each sentence, and tally totals at the top of the data frame
  for (target in targetExpressions){
    for (i in 2:nrow(details)) {
      details[i,target] = sum(gregexpr(target, details[i,"sentenceText"], ignore.case = TRUE)[[1]]>0)
      details[1,target] = details[1,target] + details[i,target]
    }
  }
  
  return(details)
}


# looping over text files and merging results ####################################################
bulkIdentifyKeywordSentences <- function(targetExpressions, fileExtension="txt", folder = "", onlySummaries = TRUE) {
  fileList = getFiles(fileExtension,folder)
  numFiles = length(fileList)
  for (i in 9001:numFiles) {
    filename=fileList[i]
    print(filename)
    # Get general data
    currFileData = identifyKeywordSentences(filename, targetExpressions)
    # Add identifiers before merging with data from other files
    currFileData$filename = filename
    currFileData$rowType = "Detail"
    currFileData$rowType[1] = "Total"
    
    # Get rid of detailed info to avoid storage issues when using huge data sources
    if (onlySummaries) {currFileData = currFileData[1,]}
    # Merge data, creating data frame on first iteration
    if (i==9001) {
      combinedData = currFileData
    } else {
      combinedData = rbind(combinedData,currFileData)
    }
    
    # Write periodically to file in case of major errors
    if (i%%1000 == 0) {
      write.csv(combinedData, paste0("first",i,"rawData.csv"))
    }
    
    # Give status updates
    if (i%%20 == 0) {
      print(paste0(round(100*i/numFiles),"% Complete (",i,"/",numFiles,")"))
    }
  }
  write.csv(combinedData, paste0("all",i,"rawData.csv"))
  return(combinedData)
}



# For symbols, look up HTML code options on dev.w3.org/html5/html-author/charref
#   e.g. Trademark has codes &trade; &TRADE; &#x02122; and &#8482; so have one of the
#     terms be "&trade;|&#x02122|&#8482;" (search is case insensitive, so you can
#     leave out the capitalized version of &trade;)

setwd("Q:/projects/uva downloads")
targetExpressions = c("launch", "patent", "trademark", "&trade;|&#x02122|&#8482;")
testOutput = bulkIdentifyKeywordSentences(targetExpressions, "txt.gzip")




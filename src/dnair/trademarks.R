#install.packages("XML")
#getwd()
# setwd("~/Documents/UVA R")
#temp <- tempfile(tmpdir = tdir <- tempdir())

# doc <- xmlInternalTreeParse("Documents/UVA R/temp/apc180325.xml", useInternalNodes = TRUE)
# doc2 <- xmlParse("Documents/UVA R/temp/apc180325.xml", useInternalNodes = FALSE)

library(XML)
temp <- tempfile(tmpdir = tdir <- "~/Documents/UVA R/temp/")
download.file("https://bulkdata.uspto.gov/data/trademark/dailyxml/applications/apc180325.zip", temp)
xmlfiles <- unzip(temp, exdir = tdir)
doc <- xmlInternalTreeParse(xmlfiles[1], useInternalNodes = TRUE)
#unlink(tdir, T,T)

# Metadata?
version_details <- xmlToDataFrame(nodes = getNodeSet(doc, "//version"))
datecreated <- xmlToDataFrame(nodes = getNodeSet(doc, "//creation-datetime"))
file_text <- xmlToDataFrame(nodes = getNodeSet(doc, "//application-information/file-segments/file-segment"))

# Actual Case Data
action_key <- xmlToDataFrame(nodes = getNodeSet(doc, "//application-information/file-segments/action-keys/action-key"))
case_file <- xmlToDataFrame(nodes = getNodeSet(doc, "//application-information/file-segments/action-keys/case-file"))
header <- xmlToDataFrame(nodes = getNodeSet(doc, "//application-information/file-segments/action-keys/case-file/case-file-header"))
event_statements <- xmlToDataFrame(nodes = getNodeSet(doc, "//application-information/file-segments/action-keys/case-file/case-file-event-statements/case-file-event-statement"))
## going back and forth on the submitted trademark
## unique(test7$`description-text`)
classifications <- XML:::xmlToDataFrame(nodes = getNodeSet(doc, "//application-information/file-segments/action-keys/case-file/classifications/classification/international-code-total-no"))
classifications2 <- XML:::xmlToDataFrame(nodes = getNodeSet(doc, "//application-information/file-segments/action-keys/case-file/classifications/classification/us-code"))
## not working because of duplicates
## works if you get field specific
correspondent <- xmlToDataFrame(nodes = getNodeSet(doc, "//application-information/file-segments/action-keys/case-file/correspondent"))
owner <- xmlToDataFrame(nodes = getNodeSet(doc, "//application-information/file-segments/action-keys/case-file/case-file-owners/case-file-owner"))
madrid <- xmlToDataFrame(nodes = getNodeSet(doc, "//application-information/file-segments/action-keys/case-file/madrid-international-filing-requests/madrid-international-filing-record"))


tm_app_cols <- c(
  colnames(version_details),
  colnames(datecreated),
  colnames(file_text),
  colnames(action_key),
  colnames(case_file),
  colnames(header),
  colnames(event_statements),
  colnames(correspondent),
  colnames(owner),
  colnames(madrid)
)

library(readr)
library(magrittr)

write_csv(as.data.frame(tm_app_cols), path = "Documents/UVA R/tm_app_cols.csv")

case_file %>% head() %>%
  left_join

duplicated(colnames(case_file),
           colnames(header))

#######


#class(doc6)
# xmldataframe <- xmlToDataFrame("Documents/UVA R/temp/apc180325.xml")
#colnames(xmldataframe)

root <- xmlRoot(doc)
root2 <- XML::xmlRoot(doc2)

children <- xmlChildren(root)
children2 <- xmlChildren(root2)

names <- xmlSApply(root, names)
attr <- xmlSApply(root, xmlAttrs)
vers_leng <- xmlSApply(root[[1]], length)
date_leng <- xmlSApply(root[[2]], length)
app_leng <- xmlSApply(root[[3]], length)

names
attr
vers_leng
date_leng
app_leng

names2 <- xmlSApply(root2, names)
attr2 <- xmlSApply(root2, xmlAttrs)
vers_leng2 <- xmlSApply(root2[[1]], length)
date_leng2 <- xmlSApply(root2[[2]], length)
app_leng2 <- xmlSApply(root2[[3]], length)

names2
attr2
vers_leng2
date_leng2
app_leng2

versSize <- xmlSApply(root[[1]], xmlSize)
dateSize <- xmlSApply(root[[2]], xmlSize)
appSize <- xmlSApply(root[[3]], xmlSize)

versSize2 <- xmlSApply(root2[[1]], xmlSize)
dateSize2 <- xmlSApply(root2[[2]], xmlSize)
appSize2 <- xmlSApply(root2[[3]], xmlSize)

versVal <- xmlSApply(root[[1]], xmlValue)
dateVal <- xmlSApply(root[[2]], xmlValue)
appVal <- xmlSApply(root[[3]], xmlValue)

versVal2 <- xmlSApply(root2[[1]], xmlValue)
dateVal2 <- xmlSApply(root2[[2]], xmlValue)
appVal2 <- xmlSApply(root2[[3]], xmlValue)

library(stringr)

str_extract(appVal, "Inc.")

print("doc")
class(doc)
is.list(doc)
print("doc2")
class(doc2)
is.list(doc2)


xmlSize(root2)


xmlSize(root)

children <- xmlChildren(root)
xmlSize(children)

children2 <-xmlChildren(root2)
xmlSize(children2)


children <- XML::xmlChildren(root) # problematic? deprecated?
# subn <- xmlChildren(children) ?
# grandchildren_1 <- xmlChildren(children[1])
## can't use children on children???

class(root)
class(children)

toproot # didn't do anything with this yet
XML::xmlName(root[[1]])
XML::xmlName(root[[1]][[1]])
XML::xmlName(root[[1]][[2]])
XML::xmlName(root[[2]])
XML::xmlName(root[[3]])


nodenames <- XML::xmlName(root)
nodeattr <- XML:: xmlAttrs(root)
names(root[[1]])
names(root[[2]])
tm_text = root[[ 2 ]]
names(tm_text)
xmlValue(tm_text[[1]])
xmlSApply(tm_text[[1]], xmlValue)

xmldataframe <- xmlToDataFrame(xmlfiles)
xmldataframe <- xmlToDataFrame(doc2)

get_title <- function(node) xpat(node, "//party", xmlValue)
xpathApply(doc2, "/PubmedArticleSet/PubmedArticle", get_title)
class(root)
#XML::saveXML(doc = doc2, file=NULL, compression=0, indent=TRUE, prefix = '\n',
#             doctype = NULL, encoding = getEncoding(doc2))


data <- xmlSApply(root,function(x) xmlSApply(x, xmlValue))



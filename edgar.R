# SEC FILE SEARCH

## LOAD FUNCTIONS ----------------------------------------------
R.utils::sourceDirectory("functions")

## SET PARAMETERS ----------------------------------------------
ciks <- getSEC_CIKSByType(c("2834", "3841"))
ciks <- c("78003", "310158") ## for testing
yrs <- c("2013", "2014", "2015", "2016", "2017")
frms <- c("10-K", "20-F")
search_patterns <- data.table::fread("search_patterns.csv")

## DOWNLOAD SEC FILINGS ----------------------------------------------
# getFilings(ciks, yrs, frms)

## SEARCH SEC FILINGS ----------------------------------------------
searchFilings(ciks, yrs, frms, search_patterns)
# searchFilings(ciks, yrs, frms[1], search_patterns)


files <- read.csv("Edgar_filenames.txt",sep="_", header = F)
head(files)
files_10k <- files[which(files$"V2" == "10-K"),]
dim(files_10k)
files_20f<- files[which(files$"V2" == "20-F"),]
dim(files_20f)

head(files_10k)
files_10k[,1] <- as.character(files_10k[,1])

#find the ciks with sec's for 2013 through 2015
files_10k <- files_10k[which(files_10k$V3 %in% c("2013","2014","2015")),]
dim(files_10k)

#find the ciks with sec's for all years (to remove the others)
install.packages("dplyr")
library(dplyr)
library(plyr)
files_10k_yrs <- ddply(files_10k, "V1", summarise,
               num_yrs = length(V3))

#files_10k_yrs <- files_10k %>%
#  group_by(V1) %>%  summarise(yrs = length(V3))

files_10k_3yrs <- files_10k_yrs[which(files_10k_yrs$num_yrs == 3),]
dim(files_10k_3yrs)
#374

ciks_10k <- unique(files_10k_3yrs$V1)
length(ciks_10k)
#702
#374


ciks_20f <- unique(files_20f[,1])
length(ciks_20f)
#88


## FOR TESTING PATTERNS
regex <- buildRegexPattern("&#174;", " ", " ")
f <- find_occurences(1001316, 2014, "10-K", regex)


##LOOP FOR ALL COMPANIES
length(ciks_10k)
head(ciks_10k)

regex <- buildRegexPattern("&#174;", " ", " ")
outmatrix <- matrix("NA", nrow=length(ciks_10k), ncol = 7)
head(outmatrix)
colnames(outmatrix) <- c("cik","numDrugs_2013", "numDrugs_2014", "numDrugs_2015", "Drugs_2013","Drugs_2014","Drugs_2015")
head(outmatrix)
i<-1
y <- 1
for(i in 1:length(ciks_10k)) {
  for(y in c(1:3)){
    outmatrix[i,1] <- ciks_10k[i]
    f <- find_occurences(ciks_10k[i], yrs[y], "10-K", regex)
    g <- gsub('[[:punct:] ]+',' ',unique(f))
    h <- gsub(c("SUP"),"",unique(g))
    h <- gsub(c("sup"),"",h)
    h <- gsub(c("text top"),"",h)
    h <- gsub(c("font"),"",h)
    h <- gsub(c("FONT"),"",h)
    h <- gsub(c("bold"),"",h)
    h <- gsub(c("bottom"),"",h)
    j <- gsub("174","",unique(h))
    j <- gsub("160","",j)
    j <- gsub("10pt","",j)
    j <- gsub("33em","",j)
    k <- gsub("\n","",unique(j))
    k <- gsub("In","",k)
    k <- gsub("The","",k)
    k <- gsub("div","",k)
    unique(k)
  #stripWhitespace(k)
    l <- gsub(" ","",unique(k))
    outmatrix[i,y+1] <- length(unique(l[nchar(l) > 0]))
    outmatrix[i,y+4] <- paste(unique(l[nchar(l) > 0]),collapse="_")
    print(i)
  }
}



}


## FOR TESTING PATTERNS
regex <- buildRegexPattern("&#174;", " ", " ")
f <- find_occurences(1001316, 2014, "10-K", regex)

g <- gsub('[[:punct:] ]+',' ',unique(f))
h <- gsub(c("SUP"),"",unique(g))
j <- gsub("174","",unique(h))
k <- gsub("\n","",unique(j))
unique(k)
#stripWhitespace(k)
l <- gsub(" ","",unique(k))

unique(l)












#f <- find_occurences(1001316, 2014, "10-K", ".{20}&#174;")

temp.f <- tempfile()
cat(f, file = temp.f)
rstudioapi::viewer(temp.f)

class(f)

length(f)
length(unique(f))











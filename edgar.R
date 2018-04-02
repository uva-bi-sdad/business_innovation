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
# searchFilings(ciks, yrs, frms, search_patterns)
searchFilings(ciks, yrs, frms[1], search_patterns)




## FOR TESTING PATTERNS
regex <- buildRegexPattern("approvals", "fda", ">")
f <- find_occurences(1001316, 2014, "10-K", regex)

temp.f <- tempfile()
cat(f, file = temp.f)
rstudioapi::viewer(temp.f)


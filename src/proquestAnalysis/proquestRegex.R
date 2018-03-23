regexFromCompanyName = function(names){
  regexOut = vector("list", length(names))
  for(i in 1:length(names)){
    words = unlist(strsplit(names[i], " "))
    nwords = length(words)
    indices = sapply(nwords:1, function(x) 1:x)
    regexOut[[i]] = paste0(sapply(indices, function(x) paste0(words[x], collapse = " ")), collapse = "|")
  }
  return(unlist(regexOut))
}

fullCompanyRegex = regexFromCompanyName(unique(companyNaics$Company, by = "Company"))
fullCompanyRegex = gsub("[\\(\\)]", "",fullCompanyRegex)
i = 4
companyList[[i]]
companyRegex = regexFromCompanyName(companyList[[i]])
companyRegex
activeFile[i, ]
str_extract_all( activeFile[i, 9], companyList[[i]])
str_extract_all( activeFile[i, 9], companyRegex)

tmpMatch = str_extract_all( activeFile[i, 9], fullCompanyRegex)
tmpMatch[sapply(tmpMatch, function(x) length(x) > 0)]
fullCompanyRegex[sapply(tmpMatch, function(x) length(x) > 0)]


tmp = "Zhejiang Geely Holding Group Co"
tmp1 = "Zhejiang|Zhejiang Geely|Zhejiang Geely Holding"
str_match_all(tmp, tmp1)

grep("MINI", companyNaics$Company, value = TRUE)

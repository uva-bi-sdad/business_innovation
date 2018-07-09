# We're breaking out the count of articles by source type for each NAICS code. Then, we're doing the same filtering by each of the keywords

# For a slide, do broken out by NAICS code and only unfiltered
# roll up to 5 digit codes
# 32541, Pharmaceutical and Medicine Manufacturing
# 33911, Medical Equipment and Supplies Manufacturing

library(dplyr)
library(data.table)
library(stringr)
library(reshape)

#load data
keywords = c("launch","new product","product release")
keystrings = c(keywords, paste0(keywords, collapse = "|"))

scrapedFiles = list.files("./data/business_innovation/working/parsedProquestData", full.names = TRUE, pattern = "naics[[:digit:]]{6}")
sourceTables = list()
for (i in seq_along(scrapedFiles)){
  name = str_extract(scrapedFiles[i], "[[:digit:]]{6}")
  sourceTables[[i]] = data.table(fread(scrapedFiles[i]))[,.(count = .N), by = Source.type]
  sourceTables[[i]]$naicsCode = name
  sourceTables[[i]]$filterTerm = "unfiltered"
  setcolorder(sourceTables[[i]], c("Source.type", "naicsCode", "filterTerm", "count"))
}

allNaicsTables = do.call(rbind, sourceTables)
allNaicsTables

# Do the above but filter out articles that contain one of the given keystrings

filteredSourceTables = list()
for(j in seq_along(keystrings)){

  filterString = keystrings[j]

  for(i in seq_along(scrapedFiles)){
    name = str_extract(scrapedFiles[i], "[[:digit:]]{6}")
    sourceTables[[i]] = data.table(fread(scrapedFiles[i]))
    indices = str_detect(sourceTables[[i]]$Full.Text, filterString)
    sourceTables[[i]] = sourceTables[[i]][indices,.(count = .N), by = Source.type]
    sourceTables[[i]]$naicsCode = name
    sourceTables[[i]]$filterTerm = filterString
    setcolorder(sourceTables[[i]], c("Source.type", "naicsCode", "filterTerm", "count"))
  }

  filteredSourceTables[[j]] = do.call(rbind, sourceTables)
}

allFilteredNaicsTables = rbind(do.call(rbind, filteredSourceTables), allNaicsTables)
allFilteredNaicsTables

# For a slide, do broken out by NAICS code and only unfiltered
# roll up to 5 digit codes
# 32541, Pharmaceutical and Medicine Manufacturing
# 33911, Medical Equipment and Supplies Manufacturing


pmm = list.files("./data/business_innovation/working/parsedProquestData", full.names = TRUE, pattern = "naics32541")
mesm = list.files("./data/business_innovation/working/parsedProquestData", full.names = TRUE, pattern = "naics33911")

sourceTables = list()
for (i in seq_along(pmm)) sourceTables[[i]] = data.table(fread(pmm[i]))
allpmm = do.call(rbind, sourceTables)[,.(count = .N), by = Source.type]
allpmm$naicsCode = "NAICS 32541"
setcolorder(allpmm, c("Source.type", "naicsCode", "count"))

sourceTables = list()
for (i in seq_along(mesm)) sourceTables[[i]] = data.table(fread(mesm[i]))
allmesm = do.call(rbind, sourceTables)[,.(count = .N), by = Source.type]
allmesm$naicsCode = "NAICS 33911"
setcolorder(allmesm, c("Source.type", "naicsCode", "count"))

manufacturing = na.omit(rbind(allpmm, allmesm)[count > 10])
dcast(manufacturing, Source.type ~ naicsCode)

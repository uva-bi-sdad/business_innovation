library(rvest)
library(stringr)
library(data.table)
library(lubridate)
library(zoo)
source("./R/proquestParseFunctions.R")

# messing around
proQuestHtml = "./data/business_innovation/original/scrapedProquestData/pfizer1.html"
fieldsOfInterest = c("Subject", "Company", "Publication title", "Publication date", "Publication subject", "Source type", "Document type")


fileNames = list.files("./data/business_innovation/original/scrapedProquestData", full.names = TRUE)
fieldsOfInterest = c("Subject", "Company", "Publication title", "Publication date", "Publication subject", "Source type", "Document type")
parsedDataOutdir = "./data/business_innovation/working/parsedProquestData/"

# Parse pfizer

pfizerNames = grep("pfizer", fileNames, value = TRUE)
parsedPfizer = parseList(pfizerNames, fieldsOfInterest, reportEach = 100)
write.csv(parsedPfizer, paste0(parsedDataOutdir, "pfizer.csv"), row.names = FALSE)

# Parse png

pngNames = grep("png[0-9].html", fileNames, value = TRUE)
parsedpng = parseList(pngNames, fieldsOfInterest, reportEach = 100)
write.csv(parsedpng, paste0(parsedDataOutdir, "png.csv"), row.names = FALSE)

# Parse merck

merckNames = grep("merck", fileNames, value = TRUE)
parsedmerck = parseList(merckNames, fieldsOfInterest, reportEach = 100)
write.csv(parsedmerck, paste0(parsedDataOutdir, "merck.csv"), row.names = FALSE)

# Parse glaxosmithkline

glaxosmithklineNames = grep("glaxosmithkline", fileNames, value = TRUE)
parsedglaxosmithkline = parseList(glaxosmithklineNames, fieldsOfInterest, reportEach = 100)
write.csv(parsedglaxosmithkline, paste0(parsedDataOutdir, "gsk.csv"), row.names = FALSE)

# Parse novartis

novartisNames = grep("novartis", fileNames, value = TRUE)
parsednovartis = parseList(novartisNames, fieldsOfInterest, reportEach = 100)
write.csv(parsednovartis, paste0(parsedDataOutdir, "novartis.csv"), row.names = FALSE)

# Parse GM

generalMotorsNames = grep("general", fileNames, value = TRUE)
parsedgeneralMotors = parseList(generalMotorsNames, fieldsOfInterest, reportEach = 100)
write.csv(parsedgeneralMotors, paste0(parsedDataOutdir, "generalMotors.csv"), row.names = FALSE)

# Parse Ford

fordNames = grep("ford", fileNames, value = TRUE)
parsedford = parseList(fordNames, fieldsOfInterest, reportEach = 100)
write.csv(parsedford, paste0(parsedDataOutdir, "ford.csv"), row.names = FALSE)

# Parse Tata

tataNames = grep("tata", fileNames, value = TRUE)
parsedtata = parseList(tataNames, fieldsOfInterest, reportEach = 100)
write.csv(parsedtata, paste0(parsedDataOutdir, "tata.csv"), row.names = FALSE)

# Parse Toyota

toyotaNames = grep("toyota", fileNames, value = TRUE)
parsedtoyota = parseList(toyotaNames, fieldsOfInterest, reportEach = 100)
write.csv(parsedtoyota, paste0(parsedDataOutdir, "toyota.csv"), row.names = FALSE)

# Parse VW

volkswagenNames = grep("volkswagen", fileNames, value = TRUE)
parsedvolkswagen = parseList(volkswagenNames, fieldsOfInterest, reportEach = 100)
write.csv(parsedvolkswagen, paste0(parsedDataOutdir, "volkswagen.csv"), row.names = FALSE)

# Parse Naics 511210

parseNames = list.files("./data/business_innovation/original/scrapedProquestData/naics511210", full.names = TRUE)
parsedFile = parseList(parseNames, fieldsOfInterest, reportEach = 100)
write.csv(parsedFile, paste0(parsedDataOutdir, "naics511210.csv"), row.names = FALSE)

# Parse Naics 518210

parseNames = list.files("./data/business_innovation/original/scrapedProquestData/naics518210", full.names = TRUE)
parsedFile = parseList(parseNames, fieldsOfInterest, reportEach = 100)
write.csv(parsedFile, paste0(parsedDataOutdir, "naics518210.csv"), row.names = FALSE)

# Parse Naics 336111

parseNames = list.files("./data/business_innovation/original/scrapedProquestData/naics336111", full.names = TRUE)
parsedFile = parseList(parseNames, fieldsOfInterest, reportEach = 100)
write.csv(parsedFile, paste0(parsedDataOutdir, "naics336111.csv"), row.names = FALSE)

# Parse Naics 541711

parseNames = list.files("./data/business_innovation/original/scrapedProquestData/naics541711", full.names = TRUE)
parsedFile = parseList(parseNames, fieldsOfInterest, reportEach = 100)
write.csv(parsedFile, paste0(parsedDataOutdir, "naics541711.csv"), row.names = FALSE)

# Parse Naics 541712

parseNames = list.files("./data/business_innovation/original/scrapedProquestData/naics541712", full.names = TRUE)
parsedFile = parseList(parseNames, fieldsOfInterest, reportEach = 100)
write.csv(parsedFile, paste0(parsedDataOutdir, "naics541712.csv"), row.names = FALSE)

# Parse Naics 325412, aka pharma

parseNames = list.files("./data/business_innovation/original/scrapedProquestData/naics325412", full.names = TRUE)
parsedFile = parseList(parseNames, fieldsOfInterest, reportEach = 100)
write.csv(parsedFile, paste0(parsedDataOutdir, "naics325412.csv"), row.names = FALSE)

# Parse Naics 339114

parseNames = list.files("./data/business_innovation/original/scrapedProquestData/naics339114", full.names = TRUE)
parsedFile = parseList(parseNames, fieldsOfInterest, reportEach = 100)
write.csv(parsedFile, paste0(parsedDataOutdir, "naics339114.csv"), row.names = FALSE)

# Parse Naics 339115

parseNames = list.files("./data/business_innovation/original/scrapedProquestData/naics339115", full.names = TRUE)
parsedFile = parseList(parseNames, fieldsOfInterest, reportEach = 100)
write.csv(parsedFile, paste0(parsedDataOutdir, "naics339115.csv"), row.names = FALSE)



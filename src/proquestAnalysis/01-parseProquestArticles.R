library(rvest)
library(stringr)
library(data.table)
library(lubridate)
library(zoo)

# Function to parse each div from a proquest pull

parseProquest = function(proQuestHtml, fieldsOfInterest, reportEach = 100){

  pfizerData = read_html(proQuestHtml)
  divs = html_nodes(pfizerData, " body > div")
  n = length(divs) - 1 # the last div is for a copyright thing
  p = length(fieldsOfInterest)

  out = matrix(NA, nrow = n, ncol = p + 2)
  colnames(out) = c("Article Title", fieldsOfInterest, "Full Text")
  out = data.frame(out)

  for(i in 1:n){

    # Article title

    html_nodes(divs[i], "p") %>%
      html_attr("style") %>%
      grep("bold", .) -> titleParagraph
    out[i, 1] = (html_nodes(divs[i], "p") %>% html_text)[titleParagraph]

    # Other fields

    otherFields = html_nodes(divs[i], "p") %>% '['(grep("strong", .)) %>% html_text
    for(j in 1:p){
      txt = otherFields[grep(fieldsOfInterest[j], str_extract(otherFields, ".{1,30}"))[1]]
      out[i, j + 1] = str_extract(txt, "(?<=: )(.*)")
    }

    # Full text
    fullTextParagraphs = html_nodes(divs[i], "p") %>% html_attr("style") %>% is.na
    txt = paste((html_nodes(divs[i], "p") %>% html_text)[fullTextParagraphs], collapse = '')
    txt = gsub("\\n", "", txt)
    txt = str_extract(txt, ".{1,5000}")
    out[i, p + 2] = txt

    if((i %% reportEach) == 0) print(sprintf("Just processed record %s!", i))
  }
  return(out)
}


parseList = function(fileNames, fieldsOfInterest, reportEach = 100){
  outList = vector("list", length(fileNames))
  for(i in 1:length(fileNames)){
    print(paste0("Parsing file ", i))
    outList[[i]] = parseProquest(fileNames[[i]], fieldsOfInterest, reportEach = reportEach)
  }
  out = do.call(rbind, outList)
  out$Publication.date = as.Date(as.yearmon(paste(str_extract(out$Publication.date, "[[:alpha:]]{3}"), str_extract(out$Publication.date, "\\d{4}"))))
  out = data.table(out)
  #out = unique(out, by = "Full.Text")
  #out = unique(out, by = "Article.Title")
  return(out)
}


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


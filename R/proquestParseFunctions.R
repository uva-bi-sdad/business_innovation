library(zoo)

parseProquest = function(proQuestHtml, fieldsOfInterest, reportEach = 100){

  pfizerData = read_html(proQuestHtml)
  divs = html_nodes(pfizerData, "body > div")
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
    if(length(titleParagraph) > 0){
      out[i, 1] = (html_nodes(divs[i], "p") %>% html_text)[titleParagraph]
    }else{
      out[i, 1] = NA
    }

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
    txt = str_extract(txt, ".{1,10000}")
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

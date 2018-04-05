## Search Each File
## Load a Complete Submission File, Search for FDA APPROVAL and Extract Table
searchFilings <- function(ciks, yrs, frms, search_patterns, keep_html = F) {
  for (c in ciks) {
    for (y in yrs) {
      for(f in frms) {
        ## Build file directory
        file_dir <- sprintf("Edgar filings/%s_%s_%s/", c, toupper(f), y)
        ## If file directory exists, go
        if (dir.exists(file_dir)) {
          
          co_name <- DBI::dbGetQuery(sdalr::con_db("edgar"), sprintf("select \"COMPANY_NAME\" from master_index where \"CIK\" = '%s' limit 1", c))[[1]]
          
          file_path <- list.files(file_dir, full.names = T)[[1]]
          submission_file <- readr::read_file(file_path)
          print(sprintf("Processing %s", file_path))
          
          ## EXTRACT PATTERNS
          extract <- NA
          for (i in 1:nrow(search_patterns)) {
            print(i)
            extract <- extractRegexPattern(text = submission_file,
                                           lookfor = search_patterns[i, lookfor], 
                                           between_this = search_patterns[i, between_this], 
                                           and_this = search_patterns[i, and_this],
                                           ignore_case = search_patterns[i, ignore_case])
            if (is.na(extract) == F) break
          }
          
          ## Format Extract as data.frame (if HTML Table Convert to data.frame, else text in one cell)
          if (keep_html == F) {
            if (!is.na(extract)) {
              if (stringr::str_detect(extract, "<table")) {
                dt <- data.table::data.table(company_name = co_name)
                table_df <- data.table::as.data.table(rvest::html_table(xml2::read_html(extract),
                                                            header = T,
                                                            fill = T
                )[[1]])
  
                dtt <- data.table::rbindlist(list(dt, table_df), fill = T)
  
                assign(sprintf("%s_%s_%s", c, f, y), dtt, envir = globalenv())
              } else {
                df <- data.frame(company_name = co_name, approvals = gsub("<.*?>", "", extract))
                assign(sprintf("%s_%s_%s", c, f, y), df, envir = globalenv())
              }
            }
          } else {
            if (!is.na(extract)) {
              df <- data.frame(company_name = co_name, approvals = extract)
              assign(sprintf("%s_%s_%s", c, f, y), df, envir = globalenv())
            }  
          }
        }
      }
    }
  }
}




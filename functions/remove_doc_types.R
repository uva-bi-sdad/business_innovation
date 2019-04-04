remove_doc_types <-
  function(xml_string, types = c("GRAPHIC", "EXCEL", "ZIP", "EX-10.3", "EX-10.6")) {
    no_ns <- gsub("\\n", " ", xml_string)
    #browser()
    for (t in types) {
      find_str <- paste0("<DOCUMENT> ?<TYPE> ?", t)
      search_str <- paste0("<DOCUMENT> ?<TYPE> ?", t, ".*?</DOCUMENT>")
      found <-
        as.data.table(stringr::str_locate_all(no_ns, find_str))

      for (i in 1:nrow(found)) {
        locs <- as.data.table(stringr::str_locate(no_ns, search_str))
        st <- locs[1, start] - 1
        en <- locs[1, end] + 1
        ifelse(is.na(locs$start) == TRUE & is.na(locs$end) == TRUE, no_ns,
               no_ns <- paste0(substr(no_ns, 1, st), substr(no_ns, en, nchar(no_ns))) )
      }
    }
    no_ns
  }

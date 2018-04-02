# Get Master Indices
getMasterIndicies <- function(year_array) {
  master_indicies <- list.files("Master Index")
  out_year_array <- c()
  for (y in year_array) {
    if (!file.exists(paste0("Master Index/", y, "master.Rda"))) {
      out_year_array <- c(out_year_array, y)
    }
  }
  if (!is.null(out_year_array)) getMasterIndex(out_year_array) else print("All Indicies Downloaded")
}

getMasterIndex <- function (year.array) 
{
  if (!is.numeric(year.array)) {
    cat("Please provide valid year.")
    return()
  }
  if (nzchar(Sys.which("libcurl"))) {
    dmethod <- "libcurl"
  }
  else if (nzchar(Sys.which("wget"))) {
    dmethod <- "wget"
  }
  else if (nzchar(Sys.which("curl"))) {
    dmethod <- "curl"
  }
  else {
    dmethod <- "auto"
  }
  DownloadFile <- function(link, dfile, dmethod) {
    tryCatch({
      utils::download.file(link, dfile, method = dmethod, 
                           quiet = TRUE)
      return(TRUE)
    }, error = function(e) {
      return(FALSE)
    })
  }
  options(warn = -1)
  dir.create("Master Index")
  status.array <- data.frame()
  for (i in 1:length(year.array)) {
    year <- year.array[i]
    year.master <- data.frame()
    quarterloop <- 4
    if (year == format(Sys.Date(), "%Y")) {
      quarterloop <- ceiling(as.integer(format(Sys.Date(), 
                                               "%m"))/3)
    }
    for (quarter in 1:quarterloop) {
      dfile <- paste0("Master Index/", year, "QTR", quarter, 
                      "master.gz")
      file <- paste0("Master Index/", year, "QTR", quarter, 
                     "master")
      link <- paste0("https://www.sec.gov/Archives/edgar/full-index/", 
                     year, "/QTR", quarter, "/master.gz")
      res <- DownloadFile(link, dfile, dmethod)
      if (res) {
        R.utils::gunzip(dfile, destname = file, temporary = FALSE, 
                        skip = FALSE, overwrite = TRUE, remove = TRUE)
        cat("Successfully downloaded Master Index for year:", 
            year, "and quarter:", quarter, "\n")
        data <- gsub("'", "", readLines(file))
        header.end <- grep("--------------------------------------------------------", 
                           data)
        writeLines(data, file)
        d <- scan(file, what = list("", "", "", "", 
                                    ""), flush = F, skip = header.end, sep = "|", 
                  quiet = T)
        COMPANY_NAME <- gsub("[[:punct:]]", " ", d[[2]], 
                             perl = T)
        data <- data.frame(CIK = d[[1]], COMPANY_NAME = COMPANY_NAME, 
                           FORM_TYPE = d[[3]], DATE_FILED = d[[4]], EDGAR_LINK = d[[5]], 
                           QUARTER = quarter)
        year.master <- rbind(year.master, data)
        file.remove(file)
        status.array <- rbind(status.array, data.frame(Filename = paste0(year, 
                                                                         ": quarter-", quarter), status = "Download success"))
      }
      else {
        status.array <- rbind(status.array, data.frame(Filename = paste0(year, 
                                                                         ": quarter-", quarter), status = "Server Error"))
      }
    }
    assign(paste0(year, "master"), year.master)
    save(year.master, file = paste0("Master Index/", year, 
                                    "master.Rda"))
  }
  return(status.array)
}
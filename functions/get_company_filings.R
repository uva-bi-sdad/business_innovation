# Get Company Filings By Year and Form Type

getFilings <- function(ciks, yrs, frms) {
  ## Get SEC MasterIndicies for Selected Years
  getMasterIndicies(yrs)
  ## Get Filings
  for (c in ciks) {
    for (y in yrs) {
      for (f in frms) {
        if (!dir.exists(paste0("Edgar filings/", c, "_", f, "_", y))) {
          print(sprintf("Getting form: %s for company: %s for year %s", f, c, y))
        getFiling(y, c, f)
        } else {
          print("Form Directory Already Exists")
        }
      }
    }
  }
}

getFiling <- function (year, cik.no, form.type) 
{
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
  DownloadFile <- function(link, filename, dmethod) {
    tryCatch({
      utils::download.file(link, filename, method = dmethod, 
                           quiet = TRUE)
      return(TRUE)
    }, error = function(e) {
      return(FALSE)
    })
  }
  options(warn = -1)
  yr.master <- paste0(year, "master.Rda")
  stat.filing2 <- data.frame()
  if (file.exists(paste0("Master Index/", yr.master))) {
    load(paste0("Master Index/", yr.master))
    if (nrow(year.master) > 0) {
      if (!cik.no == "ALL") {
        year.master <- year.master[year.master$CIK == 
                                     cik.no, ]
      }
      if (nrow(year.master) == 0) {
        msg1 <- paste0("CIK No:", cik.no, " not found in file: ", 
                       yr.master)
        cat(msg1)
        return()
      }
      if (!form.type == "ALL") {
        year.master <- year.master[year.master$FORM_TYPE == 
                                     form.type, ]
      }
      if (nrow(year.master) == 0) {
        msg2 <- paste0("Form Type: ", form.type, " not Filed by ", 
                       cik.no)
        cat(msg2)
        return()
      }
      
      total.files <- nrow(year.master)
      dir.create("Edgar filings")
      progress.bar <- txtProgressBar(min = 0, max = total.files, 
                                     style = 3)
      f.type <- gsub("/", "", form.type)
      new.dir <- paste0("Edgar filings/", cik.no, 
                        "_", f.type, "_", year)
      dir.create(new.dir)
      for (i in 1:total.files) {
        LINK <- paste0("https://www.sec.gov/Archives/", 
                       year.master$EDGAR_LINK[i])
        f.type <- gsub("/", "", year.master$FORM_TYPE[i])
        dest.filename <- paste0(new.dir, "/", year.master$CIK[i], 
                                "_", f.type, "_", year.master$DATE_FILED[i], 
                                ".txt")
        res <- DownloadFile(LINK, dest.filename, dmethod)
        if (res) {
          temp.status <- data.frame(Link = LINK, Status = "Download success")
        }
        else {
          temp.status <- data.frame(Link = LINK, Status = "Server Error")
        }
        stat.filing2 <- rbind(stat.filing2, temp.status)
        setTxtProgressBar(progress.bar, i)
      }
      close(progress.bar)
      return(stat.filing2)
      
    }
    else {
      msg4 <- "Rda file is corrupted. Please re-download the master index file for the selected year using 'getMasterIndex' function."
      cat(msg4)
      return()
    }
  }
  else {
    msg5 <- paste0("Current directory does not contains ", 
                   yr.master, " file in 'Master Index' directory. Please download master index using 'getMasterIndex' function.")
    cat(msg5)
    return()
  }
}
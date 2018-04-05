# Build REGEX Pattern
buildRegexPattern <- function(lookfor, between_this, and_this) {
  sprintf("%s((?!%s).)*%s.*?%s", between_this, between_this, lookfor, and_this)
}

# Extract REGEX Pattern
extractRegexPattern <- function(text, lookfor, between_this, and_this, ignore_case = T) {
  regex_pattern <- buildRegexPattern(lookfor = lookfor, 
                                     between_this = between_this, 
                                     and_this = and_this)
  stringr::str_extract(text, stringr::regex(regex_pattern, ignore_case))
}

# Find occurences of regex in single document
find_occurences <- function(cik, yr, frm, regex) {
  file_dir <- sprintf("Edgar filings/%s_%s_%s/", cik, toupper(frm), yr)
  file_path <- list.files(file_dir, full.names = T)[[1]]
  submission_file <- readr::read_file(file_path)
  print(sprintf("Processing %s", file_path))
  s <- stringr::str_extract_all(submission_file, stringr::regex(regex, ignore_case = T))[[1]]
  paste(s, "\n\n")
}



# Get SEC Company CIK Code by SIC Type
getSEC_CIKSByType <- function(types = c("2834", "3841")) {
  data.table::setDT(readr::read_tsv("SIC.Download.txt"))[SIC %in% types, as.numeric(CIK)]
}

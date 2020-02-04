
folder <- "git/business_innovation/data/working/secmethod_DNAdata/dna_15_hl_reg_wordlists/"
reg_paths <- list.files(folder)
reg_paths <- paste0(folder, reg_paths)

if (exists("fin_o") == TRUE) rm(fin_o)

for (i in 1:length(reg_paths)) {
  o2 <- read_csv(reg_paths[i])
  
  if (exists("fin_o") == FALSE)
    fin_o <- o2
  else
    fin_o <- rbindlist(list(fin_o, o2))
}

write.csv(fin_o, "git/business_innovation/data/working/secmethod_DNAdata/dna_15_hl_reg_wordlists_all.csv")

##############################


folder <- "git/business_innovation/data/working/secmethod_DNAdata/dna_15_hl_eng_wordlists/"
reg_paths <- list.files(folder)
reg_paths_1 <- reg_paths[1:84995]
reg_paths_1 <- paste0(folder, reg_paths_1)

if (exists("fin_o") == TRUE) rm(fin_o)

for (i in 1:length(reg_paths_1)) {
  o2 <- read_csv(reg_paths_1[i])
  
  if (exists("fin_o") == FALSE)
    fin_o <- o2
  else
    fin_o <- rbindlist(list(fin_o, o2))
}

write.csv(fin_o, "git/business_innovation/data/working/secmethod_DNAdata/dna_15_hl_eng_wordlists_first84995.csv")

dna_15_hl_eng_wordlists_84995onwards <- read.csv("git/business_innovation/data/working/secmethod_DNAdata/dna_15_hl_eng_wordlists_84995onwards.csv")
dna_15_hl_eng_wordlists_first84995 <- read.csv("git/business_innovation/data/working/secmethod_DNAdata/dna_15_hl_eng_wordlists_first84995.csv")
nrow(fin_o)
nrow(dna_15_hl_eng_wordlists_84995onwards)
head(fin_o)
head(dna_15_hl_eng_wordlists_84995onwards)
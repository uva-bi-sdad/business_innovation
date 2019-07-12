library(data.table)
library(readr)
library(quanteda)

test_df <- fread("gunzip -cq business_innovation/data/original/DNA-Extract/part-000000000000.json.gz")
head(test_df)

dt <- readtext(paste0("business_innovation/data/original/DNA-Extract/part-000000000000.json.gz"))


paths <- list.files(path = "gunzip -cq business_innovation/data/original/DNA-Extract", pattern = "*.gz", full.names = TRUE)

if(exists("final_dt")) rm(final_dt)

for (p in paths){

  dt<- read_delim(paste("gunzip -cq",p,sep="}\n"))
}

str(final_dt)

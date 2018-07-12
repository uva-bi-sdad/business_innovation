library(stringr)

MDB_Parsing <- read.csv("./data/business_innovation/working/Scraping_MDB_Output/MDB_Scraping_File_Final.csv", stringsAsFactors = FALSE)

Surgical_Parsing <- read.csv("./data/business_innovation/working/Scraping_MDB_Output/MDB_Scraping_File_Final.csv", stringsAsFactors = FALSE)

Before_after <- read.csv("./data/business_innovation/working/Company_Standardization_Rules.csv", stringsAsFactors = FALSE)

Clean_Company_Names <- character(nrow(MDB_Parsing))
activeName = MDB_Parsing$Company_Name
for(i in 1:nrow(Before_after)){
  activeName = str_replace(activeName, Before_after[i,1], Before_after[i,2])
}

activeName
cbind(MDB_Parsing, activeName)

write.csv(MDB_Parsing, "./data/business_innovation/working/Medical_Devices_Briefs/Standardized_Company_Names.csv", row.names = FALSE)

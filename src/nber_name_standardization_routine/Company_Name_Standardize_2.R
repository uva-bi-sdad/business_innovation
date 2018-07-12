library(foreign)
library(stringr)

# read in nber stata files
# Corporate entity file matched to WRDS Compustat file
# NBER matched USPTO patents to the North American Compustat data at WRDS (not the Global data)
# waiting to get access to compustat through https://wrds-web.wharton.upenn.edu/wrds/
# https://finance.pamplin.vt.edu/research/rescources.html
pdpcohdr <- read.dta("data/business_innovation/original/nber/pdpcohdr.dta")
# File of patent assignees, including name and unique assignee number (PDPASS)
assignee <- read.dta("data/business_innovation/original/nber/assignee.dta")
# Dynamic match of patent assignee to corporate entity
dynass <- read.dta("data/business_innovation/original/nber/dynass.dta")


# read in stata code files with information we can use
derwent<-readLines("src/nber_name_standardization_routine/derwent_standardisation_BHH.do")
derwent<-derwent[11:length(derwent)]

str_extract_all(derwent[1], '(?<=\\")([^,]*?)(?=\\")')

str_extract_all(derwent, '(?<=\\")([^,]*?)(?=\\")', simplify = TRUE) -> derwent_before_after

head(derwent_before_after)

# read in stata code files with information we can use
name <-readLines("src/nber_name_standardization_routine/standard_name.do")
name <-name[21:length(name)]

str_extract_all(name[1], '(?<=\\")([^,]*?)(?=\\")')

str_extract_all(name, '(?<=\\")([^,]*?)(?=\\")', simplify = TRUE) -> name_before_after

head(name_before_after)
name_before_after[23]
name_before_after = name_before_after[nchar(name_before_after[,1])>0,]
head(name_before_after)

all_before_after <- rbind(name_before_after,derwent_before_after)

colnames(all_before_after) = c("Before_String", "After_String")

write.csv(all_before_after, "./data/business_innovation/working/Company_Standardization_Rules.csv", row.names = FALSE)

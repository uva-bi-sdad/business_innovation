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


nameonly<-readLines("src/nber_name_standardization_routine/nameonly_main.do")
nameonly<-nameonly[32:301]
nameonly.df<-data.frame(nameonly)
nameonly.df$nameonly<-as.character(nameonly.df$nameonly)

string<-nameonly.df$nameonly

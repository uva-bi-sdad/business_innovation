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
derwent.df<-data.frame(derwent)
derwent.df$derwent<-as.character(derwent.df$derwent)

string<-derwent.df$derwent
start1<-str_locate(string,',\" ')[,2]
end1<-str_locate(string,' \",\"')[,1]
col1<-str_sub(string,start1,end1)

start2<-str_locate(string,'\",\" ')[,2]
end2<-str_locate(string,' \",1')[,1]
col2<-str_sub(string,start2,end2)

derwent.df$col1<-col1
derwent.df$col2<-col2

# nameonly_main.do: Clean Compustat name file
# clean names using Special Compustat recoding

nameonly<-readLines("src/nber_name_standardization_routine/nameonly_main.do")
nameonly<-nameonly[32:301]
nameonly.df<-data.frame(nameonly)
nameonly.df$nameonly<-as.character(nameonly.df$nameonly)

string<-nameonly.df$nameonly

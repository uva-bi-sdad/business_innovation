#load("~/git/business_innovation/data/original/Master Index/Master Index 1996 2010/2010master.Rda")
# load("~/git/business_innovation/data/original/Master Index/Master Index_2011/2011master.Rda")

# master_index_01_05 <- year.master
# master_index_06_11 <- year.master
# master_index_96_00 <- rbind(year.master, master_index_96_97)
# saveRDS(master_index_96_00, "~/git/business_innovation/data/original/Master Index/master_index_96_00.RDS")
# master_index_01_05 <- rbind(year.master, master_index_01_05)
# master_index_06_11 <- rbind(year.master, master_index_06_11)
#saveRDS(master_index_01_05, "~/git/business_innovation/data/original/Master Index/master_index_01_05.RDS")
#unique(str_extract(master_index_96_00$DATE_FILED, "\\d{0,4}")) #"2000" "1998" "1997" "1996"
# unique(str_extract(master_index_01_10$DATE_FILED, "\\d{0,4}"))
# unique(str_extract(master_index_06_11$DATE_FILED, "\\d{0,4}"))
# saveRDS(master_index_06_11, "~/git/business_innovation/data/original/Master Index/master_index_06_11.RDS")

# load("~/git/business_innovation/data/original/Master Index/2018master.Rda")
# #rm(master_index_06_11)
# #master_index_12_18 <- year.master
# master_index_12_18 <- rbind(year.master, master_index_12_18)
# unique(str_extract(master_index_12_18$DATE_FILED, "\\d{0,4}"))
# saveRDS(master_index_12_18, "~/git/business_innovation/data/original/Master Index/master_index_12_18.RDS")


batch1 <- readRDS("~/git/business_innovation/data/original/Master Index/master_index_96_00.RDS")
batch2 <- readRDS("~/git/business_innovation/data/original/Master Index/master_index_01_05.RDS")
batch3 <- readRDS("~/git/business_innovation/data/original/Master Index/master_index_06_11.RDS")
batch4 <- readRDS("~/git/business_innovation/data/original/Master Index/master_index_12_18.RDS")
allyears <- rbind(batch1, batch2, batch3, batch4)
#saveRDS(allyears, "~/git/business_innovation/data/original/Master Index/master_index_ALL_96_18.RDS")
master_indices_all_96_18 <- readRDS("~/git/business_innovation/data/original/Master Index/master_index_ALL_96_18.RDS")
rm(batch1)
rm(batch2)
rm(batch3)
rm(batch4)

CIKNAMES <- allyears %>% count(CIK, COMPANY_NAME)
CIKNAMES
CIKNAMES <- saveRDS("~/git/business_innovation/data/working/sec/ALL_company_names_CIK_SIC/CIKNAMES_ALL_1996_2018.RDS")

sicdata <- readr::read_tsv("~/git/business_innovation/data/original/SIC.Download.txt")
sicdata
nrow(sicdata) #58,427
nrow(CIKNAMES) #677,693




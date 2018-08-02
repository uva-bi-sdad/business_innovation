library(data.table)

fda = fread("./data/business_innovation/final/FDA Medical Device/full_clean.csv")
tj = fread("./data/business_innovation/working/Scraping_MDB_Output/MDB_Scraping_File_Final.csv")

fda
tj_app = tj$Company_Name
length(tj_app)

tj_app_dirty = data.table::data.table(tj_app)
counts_tj_dirty = dplyr::arrange(tj_app_dirty[, .N, by = tj$Company_Name], -N)
nrow(counts_tj_dirty)

tj_vec = tolower(tj_app)
tj_vec <- str_trim(tj_vec, side = 'both') %>%
  str_replace_all(" \\+ ", " and ") %>%
  str_replace_all("[[:punct:]]", "") %>%
  str_replace_all(" inc$","") %>%
  str_replace_all(" pharm.*$","") %>%
  str_replace_all(" technologies$","") %>%
  str_replace_all(" intl$", "") %>%
  str_replace_all(" llc$", "") %>%
  str_replace_all(" co$", "") %>%
  str_replace_all(" north america$", "") %>%
  str_replace_all(" international$", "") %>%
  str_replace_all(" labs$", "") %>%
  str_replace_all(" products$", "") %>%
  str_replace_all("sanofiaventis", "sanofi aventis") %>%
  str_replace_all(" us$", "") %>%
  str_replace_all(" corp.*$", "") %>%
  str_replace_all(" industries$", "") %>%
  str_replace_all(" electronics$", "") %>%
  str_replace_all(" medical$", "") %>%
  str_replace_all(" cons ", " consumer ") %>%
  str_replace_all(" cons$", " consumer")%>%
  str_replace_all(" con ", " consumer ")%>%
  str_replace_all(" con$", " consumer")%>%
  str_replace_all("hlth", "health") %>%
  str_replace_all("health care", "healthcare") %>%
  str_replace_all("glaxosmithkline consumer [^h]", "glaxosmithkline consumer healthcare") %>%
  str_replace_all("philadelphia pa", "") %>%
  str_replace_all("philadelphia", "") %>%
  str_replace_all("pittsburgh", "") %>%
  str_replace_all("glaxosmithkline consumer [^h]", "glaxosmithkline consumer healthcare") %>%
  str_replace_all("philadelphia", "") %>%
  str_replace_all("philadelphia pa", "") %>%
  str_replace_all(" pa(\\s|$)", "") %>%
  str_replace_all(" england", "") %>%
  str_replace_all(" grp ltd", " grp") %>%
  str_replace_all(" denver", "") %>%
  str_replace_all("(?= health(\\s?)care)(.*)", "") %>%
  str_trim(side = 'both') %>%
  str_replace_all(" consumer healthcare.*$", " consumer healthcare") %>%
  str_replace_all(" ireland$", "") %>%
  str_replace_all("johnsonmerck", "johnson-merck") %>%
  str_replace_all("johnson johnson ", "johnson and johnson ") %>%
  str_replace_all("johnson  johnson ", "johnson and johnson ") %>%
  str_replace_all("j and j", "johnson and johnson") %>%
  str_replace_all("johnson and johnson consumer$", "johnson and johnson consumer healthcare") %>%
  str_replace_all("johnson and johnsonconsumer healthcare", "johnson and johnson consumer healthcare") %>%
  str_replace_all("(?= health(\\s?)care)(.*)", " healthcare") %>%
  str_replace_all(" and$", "")%>%
  str_replace_all(" bristolmyers squibb", "bristol myers squibb") %>%
  str_replace_all("sigmatau", "sigma tau") %>%
  str_replace_all(" ip$", "") %>%
  str_replace_all("inc$", "")

tj$new_company = tj_vec

tj_app <- subset(tj, select=c(Company_Name, new_company))

tj_app <- data.table::data.table(tj_app)
length(unique(tj_app$new_company))

#cleaning the top company names------
tj_app$new_company_renamed = tj_app$new_company %>%
  str_replace_all(".*medtronic.*","medtronic") %>%
  str_replace_all(".*st\\sjude.*", "st jude") %>%
  str_replace_all(".*boston\\sscientific.*", "boston scientific") %>%
  str_replace_all(".*abbott.*", "abbott") %>%
  str_replace_all(".*roche.*", "roche") %>%
  str_replace_all(".*livanova.*","livanova") %>%
  str_replace_all(".*johnson\\sand\\sjohnson.*", "johnson and johnson") %>%
  str_replace_all(".*siemens.*", "siemens") %>%
  str_replace_all("^ge\\s.*", "ge") %>%
  str_replace_all("stryker.*", "stryker") %>%
  str_replace_all("zimmer.*", "zimmer") %>%
  str_replace_all("covidien.*", "covidien") %>%
  str_replace_all(".*alcon.*","alcon") %>%
  str_replace_all(".*gore.*","wl gore associates") %>%
  str_replace_all(".*depuy.*","depuy") %>%
  str_replace_all(".*ev3 neurovascular.*","ev3 neurovascular") %>%
  str_replace_all(".*cr bard.*","cr bard") %>%
  str_replace_all(".*mentor.*","mentor") %>%
  str_replace_all(".*thoratech.*","thoratech") %>%
  str_replace_all(".*coopervision.*","coopervision") %>%
  str_replace_all(".*qiagen.*","qiagen") %>%
  str_replace_all(".*genprobe.*","genprobe") %>%
  str_replace_all(".*medel.*","medel") %>%
  str_replace_all(".*cook.*","cook") %>%
  str_replace_all(".*irvine.*","irvine") %>%
  str_replace_all(".*baxter.*","baxter") %>%
  str_replace_all(".*biomet.*","biomet") %>%
  str_replace_all(".*codman.*","codman") %>%
  str_replace_all(".*genzyme.*","genzyme") %>%
  str_replace_all(".*smithnephew.*","smith nephew") %>%
  str_replace_all(".*dako.*","dako") %>%
  str_replace_all(".*reshape.*","reshape") %>%
  str_replace_all(".*bd diagnostics.*","bd diagnostics") %>%
  str_replace_all(".*insightec.*","insightec") %>%
  str_replace_all("maxon.*", "maxon precision motors") %>%
  str_replace_all("techeth", "techetch") %>%
  str_replace_all("bei kimco.*", "bei kimco") %>%
  str_replace_all(".*micromedics.*", "micromedics") %>%
  str_replace_all(".*nordson.*","nordson") %>%
  str_replace_all("tdk.*","tdk") %>%
  str_replace_all(".*bayer.*","bayer") %>%
  str_replace_all(".*schnider electric motion usa.*", "schnider electric motion") %>%
  str_replace_all(".*miyachi.*", "miyachi") %>%
  str_replace_all(".*eos.*", "eos") %>%
  str_replace_all(".*delo.*", "delo")
tj_final = tj_app$new_company_renamed

saveRDS(tj_final, "./data/business_innovation/final/FDA Medical Device/tjVEC.RDS")
top10_tj = dplyr::arrange(tj_app[, .N, by = tj_app$new_company_renamed], -N)[1:10,]
counts_tj = dplyr::arrange(tj_app[, .N, by = tj_app$new_company_renamed], -N)
nrow(counts_tj)

fwrite(counts_tj, "./data/business_innovation/final/FDA Medical Device/counts_tj.csv")


FDA_vec = readRDS("./data/business_innovation/final/FDA Medical Device/fdaVEC.RDS")
TJ_vec = readRDS("./data/business_innovation/final/FDA Medical Device/tjVEC.RDS")
FDA_vec <- unique(FDA_vec)
TJ_vec <- unique(TJ_vec)


x <- adist(FDA_vec,TJ_vec)

rownames(x) = FDA_vec
colnames(x) <- TJ_vec

x[1:5,1:5]
table(x)

y <- melt(x)
y$Var1 = as.character(y$Var1)
y$Var2 = as.character(y$Var2)

y$std_dist <- 2*y$value / (nchar(y[,1]) + nchar(y[,2]))

#graphics
hist(y$std_dist)

quantile(y$std_dist,seq(0,1,length =11))

y = y[order(y$std_dist),]
colnames(y)[1:2] = c("FDA Database", "Pharmacy Times")
y = dplyr::filter(y, std_dist < 1)
View(head(y, 100))


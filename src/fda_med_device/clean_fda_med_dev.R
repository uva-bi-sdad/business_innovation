library(dplyr)
library(stringr)
#CLEAN FDA DATA SOURCES-----

master = data.table::fread("data/business_innovation/working/med_device_data/masterkey.csv")
nrow(master) #16606
length(unique(master$device_name))

#PMA-----------------------------------------------------------------------------------------
pma = dplyr::filter(master, type == "pma")
nrow(pma) #6986
length(unique(master$applicant))

pma_app = pma$applicant
#pma = data.table::data.table(pma)

#CLEAN COMPANY NAMES-------
pma_vec = tolower(pma_app)
pma_vec <- str_trim(pma_vec, side = 'both') %>%
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

pma$new_company = pma_vec

pma_app <- subset(pma, select=c(applicant, new_company, decision_date))

pma_app <- data.table::data.table(pma_app)
length(unique(pma_app$new_company))

#cleaning the top company names------
pma_app$new_company_renamed = pma_app$new_company %>%
  str_replace(".*medtronic.*","medtronic") %>%
  str_replace(".*st\\sjude.*", "st jude") %>%
  str_replace(".*boston\\sscientific.*", "boston scientific") %>%
  str_replace(".*abbott.*", "abbott") %>%
  str_replace(".*roche.*", "roche") %>%
  str_replace(".*livanova.*","livanova") %>%
  str_replace(".*johnson\\sand\\sjohnson.*", "johnson and johnson") %>%
  str_replace(".*siemens.*","siemens") %>%
  str_replace(".*alcon.*","alcon") %>%
  str_replace(".*gore.*","wl gore associates") %>%
  str_replace(".*livanova.*","livanova") %>%
  str_replace(".*depuy.*","depuy") %>%
  str_replace(".*ev3 neurovascular.*","ev3 neurovascular") %>%
  str_replace(".*cr bard.*","cr bard") %>%
  str_replace(".*mentor.*","mentor") %>%
  str_replace(".*thoratech.*","thoratech") %>%
  str_replace(".*coopervision.*","coopervision") %>%
  str_replace(".*qiagen.*","qiagen") %>%
  str_replace(".*genprobe.*","genprobe") %>%
  str_replace(".*medel.*","medel") %>%
  str_replace(".*cook.*","cook") %>%
  str_replace(".*irvine.*","irvine") %>%
  str_replace(".*baxter.*","baxter") %>%
  str_replace(".*biomet.*","biomet") %>%
  str_replace(".*codman.*","codman") %>%
  str_replace(".*genzyme.*","genzyme") %>%
  str_replace(".*smithnephew.*","smith nephew") %>%
  str_replace(".*dako.*","dako") %>%
  str_replace(".*reshape.*","reshape") %>%
  str_replace(".*bd diagnostics.*","bd diagnostics") %>%
  str_replace(".*insightec.*","insightec")



testthat::expect_equal(pma_app$new_company_renamed %>% unique() %>% str_detect(".*medtronic.*") %>% sum(),
                       1)
testthat::expect_equal(pma_app$new_company_renamed %>% unique() %>% str_detect(".*st\\sjude.*") %>% sum(),
                       1)
testthat::expect_equal(pma_app$new_company_renamed %>% unique() %>% str_detect(".*boston\\sscientific.*") %>% sum(),
                       1)
testthat::expect_equal(pma_app$new_company_renamed %>% unique() %>% str_detect(".*abbott.*") %>% sum(),
                       1)
testthat::expect_equal(pma_app$new_company_renamed %>% unique() %>% str_detect(".*roche.*") %>% sum(),
                       1)

top10_pma = dplyr::arrange(pma_app[, .N, by = pma_app$new_company_renamed], -N)[1:10,]
counts = dplyr::arrange(pma_app[, .N, by = pma_app$new_company_renamed], -N)
nrow(counts)
#SAVE INITAL-CLEAN TOP10 --------
data.table::fwrite(top10_pma, "./data/business_innovation/working/med_device_data/pma_init_clean.csv")


#ARRANGE BY TOP COMPANIES BY YEAR
pma_app$approval_year = lubridate::year(lubridate::ymd(pma_app$decision_date))
pma_app <- data.table::data.table(pma_app)

decision_by_year = pma_app[new_company_renamed %in% top10_pma$pma_app, .N, by = c("new_company_renamed", "approval_year")]
decision_by_year$approval_year = factor(decision_by_year$approval_year, levels = c("2015", "2014", "2013"))
#PLOT-------
library(ggplot2)

plot_pma = ggplot(data = decision_by_year) +
  #reorder the x-axis by greatest to smallest
  geom_bar(mapping = aes(x = forcats::fct_reorder(new_company_renamed, -N),
                         fill = approval_year,
                         y = N),
           stat = 'identity',
           color = "black") +
#position='fill') +
#position = 'dodge') +
# rotate the column names
  theme(axis.text.x  = element_text(angle=60, vjust=1, hjust=1)) +
  #table and axis names
  ggtitle("Top 10 Companies with PMA-Approved Medical Devices 2013-2015") +
  ylab("Number of Approved Medical Devices") +
  xlab("Companies")+
  #font sizes
  theme(plot.title = element_text(hjust = 0.5, size = 25)) +
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=16)) +
  # wrapping the column names at certain lenght
  scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 20)) +
  # edit legend styles
  theme(legend.text = element_text(size=22)) +
  theme(legend.title = element_text(size=22)) +
  guides(fill = guide_legend(title= "Year"))

#show plot below
#plot_pma





#510K-----------------------------------------------------------------------------------------

pre = dplyr::filter(master, type == "510k")
nrow(pre) #9274

pre_app = pre$applicant
length(unique(pre_app))

pre_app_dirty = data.table::data.table(pre_app)
top10_pre_dirty = dplyr::arrange(pre_app_dirty[, .N, by = pre$applicant], -N)[1:10,]
counts_pre_dirty = dplyr::arrange(pre_app_dirty[, .N, by = pre$applicant], -N)
nrow(counts_pre_dirty)
#4563 unique companies before cleaning

pre_vec = tolower(pre_app)
pre_vec <- str_trim(pre_vec, side = 'both') %>%
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

pre$new_company = pre_vec

pre_app <- subset(pre, select=c(applicant, new_company, decision_date))

pre_app <- data.table::data.table(pre_app)
length(unique(pre_app$new_company))

#cleaning the top company names------
pre_app$new_company_renamed = pre_app$new_company %>%
  str_replace(".*medtronic.*","medtronic") %>%
  str_replace(".*st\\sjude.*", "st jude") %>%
  str_replace(".*boston\\sscientific.*", "boston scientific") %>%
  str_replace(".*abbott.*", "abbott") %>%
  str_replace(".*roche.*", "roche") %>%
  str_replace(".*siemens.*", "siemens") %>%
  str_replace(".*johnson\\sand\\sjohnson.*", "johnson and johnson") %>%
  str_replace("^ge\\s.*", "ge") %>%
  str_replace("stryker.*", "stryker") %>%
  str_replace("zimmer.*", "zimmer") %>%
  str_replace("covidien.*", "covidien")




testthat::expect_equal(pre_app$new_company_renamed %>% unique() %>% str_detect(".*medtronic.*") %>% sum(),
                       1)
testthat::expect_equal(pre_app$new_company_renamed %>% unique() %>% str_detect(".*st\\sjude.*") %>% sum(),
                       1)
testthat::expect_equal(pre_app$new_company_renamed %>% unique() %>% str_detect(".*boston\\sscientific.*") %>% sum(),
                       1)
testthat::expect_equal(pre_app$new_company_renamed %>% unique() %>% str_detect(".*abbott.*") %>% sum(),
                       1)
testthat::expect_equal(pre_app$new_company_renamed %>% unique() %>% str_detect(".*roche.*") %>% sum(),
                       1)
testthat::expect_equal(pre_app$new_company_renamed %>% unique() %>% str_detect(".*siemens.*") %>% sum(),
                       1)
testthat::expect_equal(pre_app$new_company_renamed %>% unique() %>% str_detect("^ge\\s.*") %>% sum(),
                       0)
testthat::expect_equal(pre_app$new_company_renamed %>% unique() %>% str_detect("covidien.*") %>% sum(),
                       1)


top10_pre = dplyr::arrange(pre_app[, .N, by = pre_app$new_company_renamed], -N)[1:10,]
counts_pre = dplyr::arrange(pre_app[, .N, by = pre_app$new_company_renamed], -N)
nrow(counts_pre)
#3686


#SAVE INITAL-CLEAN TOP10 --------
data.table::fwrite(top10_pre, "./data/business_innovation/working/med_device_data/510k_init_clean.csv")


#ARRANGE BY TOP COMPANIES BY YEAR
pre_app$approval_year = lubridate::year(lubridate::ymd(pre_app$decision_date))
pre_app <- data.table::data.table(pre_app)

decision_by_year = pre_app[new_company_renamed %in% top10_pre$pre_app, .N, by = c("new_company_renamed", "approval_year")]
decision_by_year$approval_year = factor(decision_by_year$approval_year, levels = c("2015", "2014", "2013"))

#PLOT-------
library(ggplot2)
plot_pre = ggplot(data = decision_by_year) +
  #reorder the x-axis by greatest to smallest
  geom_bar(mapping = aes(x = forcats::fct_reorder(new_company_renamed, -N),
                         fill = approval_year,
                         y = N),
           stat = 'identity',
           color = "black") +
  #position='fill') +
  #position = 'dodge') +
  # rotate the column names
  theme(axis.text.x  = element_text(angle=60, vjust=1, hjust=1)) +
  #table and axis names
  ggtitle("Top 10 Companies with 510K-Substantially Equivalent \nMedical Devices 2013-2015") +
  ylab("Number of Approved Medical Devices") +
  xlab("Companies")+
  #font sizes
  theme(plot.title = element_text(hjust = 0.5, size = 25)) +
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=16)) +
  # wrapping the column names at certain lenght
  scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 20)) +
  # edit legend styles
  theme(legend.text = element_text(size=22)) +
  theme(legend.title = element_text(size=22)) +
  guides(fill = guide_legend(title= "Year"))






#HDE---------------------------------------------------------------------------------------

hde = dplyr::filter(master, type == "HDE")
nrow(hde) #346

hde = hde[!(hde$applicant == "Applicant"), ]
length(unique(hde$applicant))
hde_app = hde$applicant

hde_app_dirty = data.table::data.table(hde_app)
top10_hde_dirty = dplyr::arrange(hde_app_dirty[, .N, by = hde$applicant], -N)[1:10,]
counts_hde_dirty = dplyr::arrange(hde_app_dirty[, .N, by = hde$applicant], -N)
nrow(counts_hde_dirty)

#CLEAN COMPANY NAMES-------
hde_vec = tolower(hde_app)
hde_vec <- str_trim(hde_vec, side = 'both') %>%
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

hde$new_company = hde_vec

hde_app <- subset(hde, select=c(applicant, new_company, decision_date))

hde_app <- data.table::data.table(hde_app)
length(unique(hde_app$new_company))

#cleaning the top company names------
hde_app$new_company_renamed = hde_app$new_company %>%
  str_replace(".*medtronic.*","medtronic") %>%
  str_replace(".*st\\sjude.*", "st jude") %>%
  str_replace(".*boston\\sscientific.*", "boston scientific") %>%
  str_replace(".*abbott.*", "abbott") %>%
  str_replace(".*roche.*", "roche") %>%
  str_replace(".*siemens.*", "siemens") %>%
  str_replace(".*johnson\\sand\\sjohnson.*", "johnson and johnson") %>%
  str_replace("^ge\\s.*", "ge") %>%
  str_replace("stryker.*", "stryker") %>%
  str_replace("zimmer.*", "zimmer") %>%
  str_replace("covidien.*", "covidien")


testthat::expect_equal(hde_app$new_company_renamed %>% unique() %>% str_detect(".*medtronic.*") %>% sum(),
                       1)
testthat::expect_equal(hde_app$new_company_renamed %>% unique() %>% str_detect(".*abbott.*") %>% sum(),
                       1)

top10_hde = dplyr::arrange(hde_app[, .N, by = hde_app$new_company_renamed], -N)[1:10,]
counts_hde = dplyr::arrange(hde_app[, .N, by = hde_app$new_company_renamed], -N)
nrow(counts_hde)

#SAVE INITAL-CLEAN TOP10 --------
data.table::fwrite(top10_hde, "./data/business_innovation/working/med_device_data/hde_init_clean.csv")


#ARRANGE BY TOP COMPANIES BY YEAR
hde_app$approval_year = lubridate::year(lubridate::mdy(hde_app$decision_date))
hde_app <- data.table::data.table(hde_app)
colnames(top10_hde)[1] <- "new_company_renamed"
colnames(top10_hde)[2] <- "max"


decision_by_year = hde_app[new_company_renamed %in% top10_hde$new_company_renamed, .N, by = c("new_company_renamed", "approval_year")]
decision_by_year$approval_year = factor(decision_by_year$approval_year, levels = c("2015", "2014", "2013"))

decision_by_year = left_join(decision_by_year, top10_hde, by = "new_company_renamed")

#PLOT-------
library(ggplot2)
plot_hde = ggplot(data = decision_by_year) +
  #reorder the x-axis by greatest to smallest
  geom_bar(mapping = aes(x = forcats::fct_reorder(new_company_renamed, -max),
                         fill = approval_year,
                         y = N),
           stat = 'identity',
           color = "black") +
  #position='fill') +
  #position = 'dodge') +
  # rotate the column names
  theme(axis.text.x  = element_text(angle=60, vjust=1, hjust=1)) +
  #table and axis names
  ggtitle("Top 10 Companies with HDE Approved \nMedical Devices 2013-2015") +
  ylab("Number of Approved Medical Devices") +
  xlab("Companies")+
  #font sizes
  theme(plot.title = element_text(hjust = 0.5, size = 25)) +
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=16)) +
  # wrapping the column names at certain lenght
  scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 20)) +
  # edit legend styles
  theme(legend.text = element_text(size=22)) +
  theme(legend.title = element_text(size=22)) +
  guides(fill = guide_legend(title= "Year"))



#MASTER-------------------------------------------------------------------------------------------------
library(dplyr)
library(stringr)
master = data.table::fread("data/business_innovation/working/med_device_data/masterkey.csv")
nrow(master) #16606
length(unique(master$applicant))

hde = master %>%
  filter(type == "HDE")

hde$decision_date = as.Date(hde$decision_date, '%m/%d/%Y')

rest = master %>%
  filter(type != "HDE")
rest$decision_date = as.Date(rest$decision_date)

master= rbind(rest, hde)
master_app = master$applicant
#pma = data.table::data.table(pma)

#CLEAN COMPANY NAMES-------
master_vec = tolower(master_app)

master_vec <- str_trim(master_vec, side = 'both') %>%
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

master$new_company = master_vec

master_app <- subset(master, select=c(applicant, new_company, decision_date))

master_app <- data.table::data.table(master_app)
length(unique(master_app$new_company))

#cleaning the top company names------
master_app$new_company_renamed = master_app$new_company %>%
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

fda_final <- master_app$new_company_renamed
saveRDS(fda_final, "./data/business_innovation/final/FDA Medical Device/fdaVEC.RDS")




top10_master = dplyr::arrange(master_app[, .N, by = master_app$new_company_renamed], -N)[1:10,]
counts_master = dplyr::arrange(master_app[, .N, by = master_app$new_company_renamed], -N)
nrow(counts_master)
#SAVE INITAL-CLEAN TOP10 --------
data.table::fwrite(counts_master, "./data/business_innovation/final/FDA Medical Device/full_clean.csv")


#ARRANGE BY TOP COMPANIES BY YEAR

master_app$approval_year = lubridate::year(lubridate::ymd(master_app$decision_date))


master_app <- data.table::data.table(master_app)

decision_by_year = master_app[new_company_renamed %in% top10_master$master_app, .N, by = c("new_company_renamed", "approval_year")]
decision_by_year$approval_year = factor(decision_by_year$approval_year, levels = c("2015", "2014", "2013"))
#PLOT-------
library(ggplot2)

plot_master = ggplot(data = decision_by_year) +
  #reorder the x-axis by greatest to smallest
  geom_bar(mapping = aes(x = forcats::fct_reorder(new_company_renamed, -N),
                         fill = approval_year,
                         y = N),
           stat = 'identity',
           color = "black") +
  #position='fill') +
  #position = 'dodge') +
  # rotate the column nameshttps://analytics.bi.vt.edu/luke3874/rstudio/graphics/plot_zoom_png?width=767&height=766
  theme(axis.text.x  = element_text(angle=60, vjust=1, hjust=1)) +
  #table and axis names
  ggtitle("Top 10 Companies with FDA-Approved Medical Devices\n across All Datasets 2013-2015") +
  ylab("Number of Approved Medical Devices") +
  xlab("Companies")+
  #font sizes
  theme(plot.title = element_text(hjust = 0.5, size = 25)) +
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=16)) +
  # wrapping the column names at certain lenght
  scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 20)) +
  # edit legend styles
  theme(legend.text = element_text(size=22)) +
  theme(legend.title = element_text(size=22)) +
  guides(fill = guide_legend(title= "Year"))

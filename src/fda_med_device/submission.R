library(dplyr)
library(stringr)
#CLEAN FDA DATA SOURCES-----

master = data.table::fread("data/business_innovation/working/med_device_data/masterkey.csv")
nrow(master) #16606
length(unique(master$device_name))

#PMA-----------------------------------------------------------------------------------------
pma = dplyr::filter(master, type == "pma")
nrow(pma) #6986

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

pma <- data.table::data.table(pma)
length(unique(pma$new_company))

#CLEANING SPECIFIC COMPANY NAMES------
pma$new_company_renamed = pma$new_company %>%
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


#COUNT OF SUBMISSION BY TYPE-----
process_change = pma %>%
  filter(supplement_reason == "Process Change - Manufacturer/Sterilizer/Packager/Supplier") %>%
  select(new_company_renamed, trade_name, device_name, decision_date,supplement_reason, applicant)
nrow(process_change)


location_change = pma %>%
  filter(supplement_reason == "Location Change - Manufacturer/Sterilizer/Packager/Supplier") %>%
  select(new_company_renamed, trade_name, device_name, decision_date,supplement_reason, applicant)
nrow(location_change)

change_design = pma %>%
  filter(supplement_reason == "Change Design/Components/Specifications/Material") %>%
  select(new_company_renamed, trade_name, device_name, decision_date,supplement_reason, applicant)
nrow(change_design)

labeling_change = pma %>%
  filter(supplement_reason == "Labeling Change - Indications/instructions/shelf life/tradename" |
           supplement_reason == "Labeling Change - PAS")
nrow(labeling_change)

post_approval_study = pma %>%
  filter(supplement_reason == "Postapproval Study Protocol - OSB" |
          supplement_reason == "Postapproval Study Protocol - ODE/OIR")
nrow(post_approval_study)


other_report = pma %>%
  filter(supplement_reason == "Other Report")
nrow(other_report)

none = pma %>%
  filter(supplement_reason == "")
nrow(none)

counts = dplyr::arrange(pma[, .N, by = pma$supplement_reason], -N)
counts = data.table::data.table(counts)
data.table::fwrite(counts, "./data/business_innovation/working/med_device_data/supplement_count.csv")

#USE CLEANED DATA-------
counts_supplement = data.table::fread("./data/business_innovation/working/med_device_data/supplement_reasons.csv")
View(counts_supplement)
colnames(counts_supplement)[1] <- "pma"
colnames(counts_supplement)[2] <- "N"
counts_supplement$N <- as.integer(counts_supplement$N)
counts_supplement$pma <- as.character(counts_supplement$pma)

#PLOT------

# Library
library(tidyverse)
library(ggplot2)
# 1 - Custom markers (left)
# note: stroke exists only for shapes between 1 and 24
plot = counts_supplement %>%
ggplot(aes(x = forcats::fct_reorder(pma, N), y= N, label = N))+
  geom_segment(aes(y= 0, xend = pma, yend = N),color="skyblue", size=1.5) +
  geom_point(size = 4, color="blue", fill=alpha("orange", 0.3), alpha = 1, shape = 21, stroke=1) +
  #size can be 6*(counts_supplement$N/1000
  geom_text(color = "black", size = 4 ,aes(y =5000)) +
  #N+ value is to offset the number
  theme_light() +
  coord_flip() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()
  ) +
  xlab("Submissions Reasons") +
  ylab("Numbers of PMA Submissions")
plot

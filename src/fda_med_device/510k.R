# table of 510k clearance approved devices (class II)

library(jsonlite)
library(lubridate)
library(data.table)
library(dplyr)
#import JSON file

d = fromJSON('./data/business_innovation/original/med_device/device-510k-0001-of-0001.json')

#pull out device name, company name, decision date, and decision code
# (name the decision date approved_date because later you will
#  filter and keep only approved ones

dat = data.table(device_name = d$results$device_name,
                 generic_purpose = d$results$advisory_committee_description,
                company_name = d$results$applicant,
                approved_date = d$results$decision_date,
                decision_code = d$results$decision_code)
unique(dat$generic_purpose)
dat = mutate(dat, approved_date = ymd(approved_date))
dat = filter(dat, approved_date > ymd('2013-01-01') &
              approved_date < ymd('2015-12-31'))

View(dat)
#order by date
dat = dat[order(as.Date(dat$approved_date, format="%d/%m/%Y")),]

#only keep decision code = SESE

dat = dat %>% filter(stringr::str_detect(decision_code, 'SE'))

unique(dat$decision_code)

#now we don't need the decision_code column

nrow(dat)
View(dat)

write.csv(dat, "./data/business_innovation/working/med_device_data/approved_med_device_510k.csv", row.names = FALSE)

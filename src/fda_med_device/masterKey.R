#making a master list
library(dplyr)
library(jsonlite)
library(data.table)
library(lubridate)
library(purrr)

#data from 510K
total_data = fromJSON(here::here("./data/business_innovation/original/med_device/device-510k-0001-of-0001.json"))
names(total_data$results)

results <- total_data$results %>%
  select(-openfda)
ofda = total_data$results$openfda
ofda = subset(ofda, select = -c(device_name))
df <- cbind(results, ofda)

# recode missing string to NA
df[df == ''] <- NA

#filter the date range
in_range = mutate(df, decision_date = ymd(decision_date))
in_range = filter(df, decision_date > ymd('2013-01-01') &
                    decision_date < ymd('2015-12-31'))
premarket_notif = in_range
premarket_notif$type <- "510k"



#data from PMA
total_data = fromJSON(here::here("./data/business_innovation/original/med_device/device-pma-0001-of-0001.json"))

results <- total_data$results %>%
  select(-openfda)
ofda <- total_data$results$openfda

df <- cbind(results, ofda)

testthat::expect_equal(ncol(df), ncol(results) + ncol(ofda))

# recode missing string to NA
df[df == ''] <- NA

#filter the date range
pma_in_range = mutate(df, decision_date = ymd(decision_date))
pma_in_range = filter(df, decision_date > ymd('2013-01-01') &
                    decision_date < ymd('2015-12-31'))
pma = pma_in_range
names(pma)[names(pma) == 'street_1'] <- 'address_1'
names(pma)[names(pma) == 'street_2'] <- 'address_2'
pma$zip_ext = NULL
pma$type <- "pma"


#data form HDE
hde = read.csv(file = here::here("./data/business_innovation/working/med_device_data/approved_med_device_hde.csv"), header = TRUE, sep = ",")

# recode missing string to NA
hde[hde == ''] <- NA
colnames(hde)[2] <- "applicant"
colnames(hde)[4] <- "decision_date"
hde$type <- "HDE"

pma = data.table(pma)
pre = data.table(premarket_notif)
hde = data.table(hde)

comb = rbind(pma, pre, hde, fill= TRUE)

registration = data.table(product = rep(comb$trade_name, times = sapply(comb$registration_number, length)),
                                       registrationNo = unlist(comb$registration_number))

comb[, registration_number := NULL]

fei = data.table(product = rep(comb$trade_name, times = sapply(comb$fei_number, length)),
                          feiNo = unlist(comb$fei_number))

comb[, fei_number := NULL]

fwrite(comb, file = "./data/business_innovation/working/med_device_data/masterkey.csv")


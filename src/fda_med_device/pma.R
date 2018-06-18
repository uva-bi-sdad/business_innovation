#table of pma approved devices (class III)

library(jsonlite)
library(lubridate)
library(data.table)
library(dplyr)

#import JSON file
pma = fromJSON('./data/business_innovation/original/med_device/device-pma-0001-of-0001.json')


#  filter and pull out only the columns that are necessary
dt = data.table(device_name = pma$results$trade_name,
                company_name = pma$results$applicant,
                approved_date = pma$results$decision_date,
                decision_code = pma$results$decision_code)

#change the approved_date column to ymd form and assign date parameters
dt = mutate(dt, approved_date = ymd(approved_date))
dt = filter(dt, approved_date > ymd('2013-01-01') &
              approved_date < ymd('2015-12-31'))

#order by date
dt = dt[order(as.Date(dt$approved_date, format="%d/%m/%Y")),]

#only keep decision code = APPR
dt = dt[dt$decision_code == 'APPR',]

#now we don't need the decision_code column
dt = subset(dt, select = -c(decision_code))

nrow(dt)

pma_data = dt
View(pma_data)
write.csv(pma_data, "./data//business_innovation/working/med_device_data/approved_med_device_pma.csv", row.names = FALSE)

library(dplyr)
library(xml2)
library(rvest)

url = 'https://www.accessdata.fda.gov/scripts/cdrh/cfdocs/cfHDE/hde.cfm?start_search=1&applicant=&tradename=&productcode=&hdenumber=&supplementnumber=&advisorycommittee=&docketnumber=&supplementtype=&expeditedreview=&ivdproducts=off&combinationproducts=off&decisiondatefrom=01%2F01%2F2013&decisiondateto=12%2F31%2F2015&noticedatefrom=&noticedateto=&znumber=&center=&PAGENUM=500'

getTable = function(url) {
  output = read_html(x = url) %>%
    html_table() %>%
    getElement(name = 1L)
}


. = read_html(x = url) %>%
  html_nodes('table') %>%
  .[[4]] %>%
  html_table(fill=TRUE)

. = tail(., -3)
View(.)


. = subset(., select = -c(X5, X6, X7))
View(.)
. = .[order(as.Date(.$X4, format="%d/%m/%Y")),]
View(.)
hde_data = .

colnames(hde_data)[1] = "device_name"
colnames(hde_data)[2] = "company_name"
colnames(hde_data)[3] = "HDE_code"
colnames(hde_data)[4] = "approved_date"

View(hde_data)
nrow(hde_data)
saveRDS(hde_data, "./data/business_innovation/original/fda_med_device/hde_data")

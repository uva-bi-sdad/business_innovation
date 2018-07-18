### create a column that gets the first word of the body text
library(RCurl)
library(xml2)
library(stringr)
library(rvest)
library(jsonlite)
library(dplyr)

### GET COMPANY NAME AND CREATE FREQUENCY
companyNamesA <- vector("list", 500)

urlsLighting <- str_c('https://www.surgicalproductsmag.com/product-categories/lighting-or-visualization-diagnostics?page=',0:35)
companyNamesLighting <- vector("list", length(urlsLighting))

for(i in length(urlsLighting)) {
  page <- read_html(urlsLighting[i])
  aLighting <- html_nodes(page, '.views-field-field-company-profile-reference a') %>%
    html_text()
  companyNamesLighting[[i]] = aLighting
}

urlsOr <- str_c('https://www.surgicalproductsmag.com/product-categories/or-equipment-accessories?page=', 0:60)

for(i in 1:1) {
  page <- read_html(urlsOr[i])
  aOr <- html_nodes(page, '.views-field-field-company-profile-reference a') %>%
    html_text()
}
aOr

i = 2
page <- read_html(urlsOr[i])
aOr2 <- html_nodes(page, '.views-field-field-company-profile-reference a') %>%
  html_text()
aOr2

i = 3
page <- read_html(urlsOr[i])
aOr3 <- html_nodes(page, '.views-field-field-company-profile-reference a') %>%
  html_text()
aOr3

i = 4
page <- read_html(urlsOr[i])
aOr4 <- html_nodes(page, '.views-field-field-company-profile-reference a') %>%
  html_text()
aOr4

i = 5
page <- read_html(urlsOr[i])
aOr5 <- html_nodes(page, '.views-field-field-company-profile-reference a') %>%
  html_text()
aOr5

i = 6
page <- read_html(urlsOr[i])
aOr6 <- html_nodes(page, '.views-field-field-company-profile-reference a') %>%
  html_text()
aOr6

i = 7
page <- read_html(urlsOr[i])
aOr7 <- html_nodes(page, '.views-field-field-company-profile-reference a') %>%
  html_text()
aOr7

i = 8
page <- read_html(urlsOr[i])
aOr8 <- html_nodes(page, '.views-field-field-company-profile-reference a') %>%
  html_text()
aOr8

i = 9
page <- read_html(urlsOr[i])
aOr9 <- html_nodes(page, '.views-field-field-company-profile-reference a') %>%
  html_text()
aOr9

i = 10
page <- read_html(urlsOr[i])
aOr10 <- html_nodes(page, '.views-field-field-company-profile-reference a') %>%
  html_text()
aOr10

i = 11
page <- read_html(urlsOr[i])
aOr11 <- html_nodes(page, '.views-field-field-company-profile-reference a') %>%
  html_text()
aOr11

i = 12
page <- read_html(urlsOr[i])
aOr12 <- html_nodes(page, '.views-field-field-company-profile-reference a') %>%
  html_text()
aOr12

i = 13
page <- read_html(urlsOr[i])
aOr13 <- html_nodes(page, '.views-field-field-company-profile-reference a') %>%
  html_text()
aOr13

i = 14
page <- read_html(urlsOr[i])
aOr14 <- html_nodes(page, '.views-field-field-company-profile-reference a') %>%
  html_text()
aOr14

i = 15
page <- read_html(urlsOr[i])
aOr15 <- html_nodes(page, '.views-field-field-company-profile-reference a') %>%
  html_text()
aOr15

i = 16
page <- read_html(urlsOr[i])
aOr16 <- html_nodes(page, '.views-field-field-company-profile-reference a') %>%
  html_text()
aOr16

i = 17
page <- read_html(urlsOr[i])
aOr17 <- html_nodes(page, '.views-field-field-company-profile-reference a') %>%
  html_text()
aOr17

companyName <- c(aOr, aOr2, aOr3, aOr4, aOr5, aOr6, aOr7, aOr8, aOr9, aOr10, aOr11, aOr12,
                 aOr13, aOr14, aOr15, aOr16, aOr17)

companyName

companyName <- data.frame(companyName)
write.csv(companyName, './data/business_innovation/working/SurgicalProductCompanyNames.csv', row.names = FALSE)

# get the frequency of company names
company.Freq <- table(companyName)
barplot(sort(company.Freq, decreasing = TRUE))
companyTable <- data.frame(company.Freq) %>%
  arrange(-Freq)
companyTable = companyTable[1:10,]


# filter out top 10 compnay and plot them
#for all years combined
top10Companies =  arrange(in_range[, .N, by = c("applicant")], -N)[1:10,]
in_range$approval_year = year(ymd(in_range$decision_date))
in_range = data.table(in_range)
decision_by_year = in_range[applicant %in% top10Companies$applicant, .N, by = c("applicant", "approval_year")]
decision_by_year$approval_year = factor(decision_by_year$approval_year, levels = c("2013", "2014", "2015"))
max = in_range[, .N, by = c("applicant")]


# plot stacking bar graphs for top 10 companies
library(ggplot2)
##ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy))
ggplot(data = companyTable) +
  geom_bar(mapping = aes(x =forcats::fct_reorder(companyName, -Freq),
                        fill = Freq,
                         y = Freq), stat = 'identity') +
  theme(axis.text.x  = element_text(angle=45, vjust=1, hjust=1)) + theme(axis.text = element_text(size = 14)) +
  scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 20)) +
  labs(title = "Surgical Products Mag", x = "Company Names", y = "Frequency", fill = 'frequency')

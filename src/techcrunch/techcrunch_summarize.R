library(stringr)
library(dplyr)

# summary techcrunc data
setwd("/Users/bpires/Documents/NSFInnovation-TechCrunch")
tc<-read.csv("techcrunc_clean.csv",stringsAsFactors = F)

tc$day<-str_sub(tc$date,9,10)
tc$month<-str_sub(tc$date,6,7)

# summarize average articles published per year
tc_summ<-group_by(tc,year,month,day) %>%
  summarise(total=n())
tc_summ<-group_by(tc_summ,year) %>%
  summarise(avg=mean(total),sd=sd(total),median=median(total))

# summarize average articles with innovation key worlds per year
tc_summi<-filter(tc,new_prod==1) %>%
  group_by(.,year,month,day) %>%
  summarise(total=n())
tc_summi<-group_by(tc_summi,year) %>%
  summarise(avg=mean(total),sd=sd(total),median=median(total))

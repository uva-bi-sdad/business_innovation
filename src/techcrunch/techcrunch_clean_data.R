library(dplyr)
library(stringr)
library(ggplot2)
library(reshape2)

setwd("~/Documents/NSFInnovation-TechCrunch/")
# upload tech crunch data that was scraped
# inludes all articles in tech crunch (2005 - 2018/01/24)
tech<-read.csv("scraperProductLaunch/techcrunchproducts.csv",stringsAsFactors = F)
tmp<-tech[36:nrow(tech),]
colnames(tmp)<-tmp[1,]
tmp<-tmp[-1,]

tmp2<-tech[1:35,]
tmp2<-select(tmp2,url,subtitle,title,tags,date,text)

techn<-rbind(tmp,tmp2)


# look for company and product
# choose one company and see how they talk about new products (product, launch,)
# counts per company, and times
# and then move onto other companies
# company reports may show new products/releases
# take top tech companies
# how many articles per company per year?
# filter by keywords -- counts by keyword

# import top 50 crunch base companies
comp<-read.csv("CrunchBaseTopCompanies.csv",stringsAsFactors = F)

# get top companies from number of times they are tagged
# create list with all tags
tags<-techn$tags
tags<-unlist(strsplit(tags, ";"))

tags_agg<-as.data.frame(table(tags))
# remove any extra spaces
tags_agg$tags<-str_trim(tags_agg$tags) 
tags_agg<-group_by(tags_agg,tags) %>% summarise(Freq=sum(as.numeric(Freq)))

# sort descending
tags_agg<-arrange(tags_agg,desc(Freq))

# count number of times top crunchbase companies were tagged
tags_agg$cbComp<-ifelse(tags_agg$tags %in% comp$Organization.Name,"CBTopCompany","")

# manually pick the top 50 companies out of the list of tags
write.csv(tags_agg,"tags.csv")
# upload updated csv
tags_agg2<-read.csv("tags_v2.csv",stringsAsFactors = F)

techn$company<-""

# look for company names in tags
for(i in 1:nrow(comp)) {
  company<-comp$Organization.Name[i]
  techn$company<-ifelse(str_detect(techn$tags,regex(company,ignore_case = T)),company,techn$company)
}


# link tags info
# get max number of tags
tmp<-max(str_count(techn$tags, ";"))
#techn$tag_cnt<-str_count(techn$tags, ";")
# create columns to store each tag (4 columns is the most number of tags for any article is 4)

techn$tagsl<-str_split(techn$tags, "; ")

#Ian's stuff
#splitFun = function(char){
#  unlist(strsplit(char, ";"))
#}

# function to get tags
get_tag <- function(tag_list, position) {
  if (length(tag_list) < position) {
    return(NA)
  } else {
    return(tag_list[position])
  }
}

get_tag(c("thing1", "thing2"), 1)
get_tag(c("thing1", "thing2"), 2)
get_tag(c("thing1", "thing2", "thing3"), 3)
get_tag(c("thing1", "thing2", "thing3"), 4)

# get second tag
techn$tag1 <- lapply(X = techn$tagsl, FUN = get_tag, 1)
techn$tag2 <- lapply(X = techn$tagsl, FUN = get_tag, 2)
techn$tag3 <- lapply(X = techn$tagsl, FUN = get_tag, 3)
techn$tag4 <- lapply(X = techn$tagsl, FUN = get_tag, 4)
techn$tag5 <- lapply(X = techn$tagsl, FUN = get_tag, 5)

techn$tag1<-as.character(techn$tag1)
techn$tag2<-as.character(techn$tag2)
techn$tag3<-as.character(techn$tag3)
techn$tag4<-as.character(techn$tag4)
techn$tag5<-as.character(techn$tag5)

techn$id <- seq.int(nrow(techn))
techn$tag1[techn$tag1=="NA"]<-""
techn$tag2[techn$tag2=="NA"]<-""
techn$tag3[techn$tag3=="NA"]<-""
techn$tag4[techn$tag4=="NA"]<-""
techn$tag5[techn$tag5=="NA"]<-""
techn<-techn[-8]

# write clean tech crunch file
write.csv(techn,"~/Documents/NSFInnovation-TechCrunch/techcrunch_clean.csv")

techTags<-select(techn,id,tag1,tag2,tag3,tag4,tag5)
techTags<-melt(techTags,id=c("id"))

techTags<-filter(techTags,value!="")
techTags<-filter(techTags,value!="NA")

techTags2<-left_join(techTags,tags_agg2,by=c("value"="tags"))

# write tags to file
write.csv(techTags2,"~/Documents/NSFInnovation-TechCrunch/techcrunch_tags.csv")

# search word launch
launch<-techn[str_detect(techn$title,"launch") | str_detect(techn$title,"Launch"),]
# count by company
launchTags<-left_join(techTags2,launch,by="id")

# plot top tags
# filter for tags that are company names
tags_comp_l<-filter(launchTags,company.x==TRUE)
tags_agg<-group_by(tags_comp_l,value) %>% summarise(total=n())
tags_agg<-arrange(tags_agg,desc(total))
#tags_comp_top_l<-tags_comp_top_l[1:50,]
tags_agg$value<-factor(tags_agg$value, levels=rev(tags_agg$value))

ggplot(tags_agg) +
  geom_bar(aes(x=value,y=total),stat="identity") +
  #geom_bar(data=filter(tags_comp_top,cbComp=="CBTopCompany"),aes(x=tags,y=Freq),fill="blue",stat="identity") +
  coord_flip()

# look at more recent data (counts by year)
# link tags to list of all companies to get NAIC codes  

# plot top tags
# filter for tags that are company names
tags_comp<-filter(tags_agg2,company==TRUE)
tags_comp_top<-arrange(tags_comp,desc(Freq))
tags_comp_top<-tags_comp_top[1:50,]
tags_comp_top$tags<-factor(tags_comp_top$tags, levels=rev(tags_comp_top$tags))

ggplot(tags_comp_top) +
  geom_bar(aes(x=tags,y=Freq),stat="identity") +
  geom_bar(data=filter(tags_comp_top,cbComp=="CBTopCompany"),aes(x=tags,y=Freq),fill="blue",stat="identity") +
  coord_flip()

# plot frequency of company names in tags
tags_cb<-filter(tags_agg2,cbComp=="CBTopCompany")
tags_cb$tags<-factor(tags_cb$tags, levels=rev(tags_cb$tags))

ggplot(tags_cb) +
  geom_bar(aes(x=tags,y=Freq),stat="identity") +
  coord_flip()


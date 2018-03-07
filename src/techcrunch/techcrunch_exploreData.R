# Explore tech crunch data
library(dplyr)
library(stringr)
library(reshape2)
library(ggplot2)
library(tm)
library(SnowballC)
library(wordcloud)
library(tigris)
library(viridis)
library(scales)

setwd("~/Documents/NSFInnovation-TechCrunch/")

# upload cleaned data
tech<-read.csv("~/Documents/NSFInnovation-TechCrunch/techcrunch_clean.csv",stringsAsFactors = F)
tags<-read.csv("~/Documents/NSFInnovation-TechCrunch/techcrunch_tags.csv",stringsAsFactors = F)
comps<-read.csv("~/Documents/NSFInnovation-TechCrunch/cikSicNaicsCrosswalk.csv",stringsAsFactors = F)
# search for companies in NAIC code 511210
compNaic<-read.csv("511210NAICS.csv",stringsAsFactors = F)
compNaic$company_name_short<-trimws(compNaic$company_name_short, which = c("both"))
compNaic$company_name_short<-tolower(compNaic$company_name_short)
compNaic$company_name_short<-ifelse(compNaic$company_name_short=="3d","3D",compNaic$company_name_short)

tech$text<-tolower(tech$text)
tech$year<-as.numeric(substr(tech$date,1,4))

keywords<-c("launch","new product","product release")

for (i in 1:length(keywords)) {
  tmp<-tech[str_detect(tech$text,keywords[i]),]
  tmp<-tmp$id
  if (i==1) {
    articles<-tmp
  } else {
    articles<-c(articles, tmp) 
  }
}

# for (i in 1:length(compNaic$company_name_short)) {
#   tmp<-tech[str_detect(tech$text,paste0("\\b",compNaic$company_name_short[i],"\\b")),]
#   tmpId<-tmp$id
#   tmpComp<-compNaic$company_name_short[i]
#   if (i==1) {
#     articlesId<-tmpId
#     articlesComp<-tmpComp
#   } else {
#     articlesId<-c(articlesId, tmp)
#     articlesComp<-c(articlesComp, tmp) 
#   }
# }

technp<-tech
technp$new_prod<-ifelse(technp$id %in% articles,1,0)
#technp$naicComp511210<-ifelse(technp$id %in% articlesId,1,0)
#technp$naicCompName<-ifelse(technp$id %in% articlesComp,articlesComp,"")
#write.csv(technp,"techcrunc_clean.csv")
# link to tags
techtags<-left_join(select(technp,id,year,new_prod,naicComp511210),tags,by="id")
# remove nas
#techtags<-filter(techtags,!is.na(X))
techtags<-select(techtags,id,year,value,company,new_prod,naicComp511210)

# keep tags that are company names
#techtags_comp<-filter(techtags,company==TRUE)
#techtags_comp<-select(techtags_comp,id,value,year)

# plot over time of articles with keywords
techtags_summ<-distinct(filter(techtags,new_prod==1),id,year) %>% group_by(.,year) %>% summarise(new_prd_cnt=n())
tech_summ<-distinct(technp,id,year) %>% group_by(.,year) %>% summarise(total=n())
tech_yr<-left_join(techtags_summ,tech_summ,by="year")
tech_yr$per_new_prod<-tech_yr$new_prd_cnt/tech_yr$total
tech_yr$other_cnt<-tech_yr$total-tech_yr$new_prd_cnt
tech_yrm<-melt(tech_yr,id=c("year","total","per_new_prod"))
tech_yrm$variable<-factor(tech_yrm$variable,levels=c("other_cnt","new_prd_cnt"))

# by year
pal <- rev(viridis_pal(alpha = 1, begin = 0, end = 1, direction = 1,
                       option = "A")(10))[c(4,8)] 
show_col(pal)

pdf("new_prod_articles_year.pdf",width=6,height=5)
ggplot(tech_yrm) +
  geom_bar(aes(x=year,y=value,fill=variable),stat="identity") +
  theme_bw() +
  scale_fill_manual(values=pal,name = "",labels = c("All Other Articles", "New Product Articles")) +
  theme(axis.text.x=element_text(size=14),
        axis.text.y=element_text(size=14),
        axis.title=element_text(size=16),
        title=element_text(size=20),
        legend.position="bottom",
        legend.text = element_text(size=16)) +
  labs(x="Year",y="Number of Articles")
dev.off()

# by tags - word cloud of articles with no new product and one with new products
tags_summ<-group_by(techtags,new_prod,value) %>%
  summarise(count=n())
tags_summ<-filter(tags_summ,!is.na(value))

new_articles<-filter(tags_summ,new_prod==1)
other_articles<-filter(tags_summ,new_prod==0)

pdf("wordclouds_tags.pdf",width=12,height=6)
par(mfrow=c(1,2))
#text(x=0.5, y=0.5, "New Product Articles")
wordcloud(new_articles$value, new_articles$count, max.words = 250, random.order = FALSE)
wordcloud(other_articles$value, other_articles$count, max.words = 250, random.order = FALSE)
dev.off()

# side by side bar chart of top companies over time (articles with new prods)
top_comp<-filter(techtags,company==TRUE)
top_comp_summ<-group_by(top_comp,value) %>%
  summarise(count=n())
top_comp_summ<-arrange(top_comp_summ,desc(count))[1:9,1]
top_comp<-filter(top_comp,value %in% top_comp_summ$value)
top_comp<-group_by(top_comp,value,year,new_prod) %>%
  summarise(count=n())
top_comp$value<-factor(top_comp$value,levels=(top_comp_summ$value))
top_comp$new_prod2<-ifelse(top_comp$new_prod==1,"New Product Article","Other Article")
top_comp$new_prod2<-factor(top_comp$new_prod2,levels=c("Other Article","New Product Article"))

position = position_stack(vjust = 1)
label <- group_by(top_comp,value,year) %>%
  summarise(total=sum(count))
top_comp <- left_join(top_comp,label,by=c("value","year"))

pdf("top_companies.pdf",width=16,height=8)
ggplot(top_comp) +
  geom_bar(aes(x=year,y=count,fill=new_prod2,group=new_prod2),stat="identity",position=position) +
  geom_text(aes(x=year,y=count,group = new_prod2,label=ifelse(new_prod==0,total,"")),size=5,vjust=0,position = position) +
  facet_wrap(~value,ncol=3) +
  scale_y_continuous(limits=c(0,1050))+
  theme_bw() +
  scale_fill_manual(values=pal,name = "",labels = c("All Other Articles", "New Product Articles")) +
  labs(x="Year",y="Number of Articles") +
  theme(strip.text = element_text(size=18),
        axis.title=element_text(size=18),
        axis.text=element_text(size=16),
        title=element_text(size=20),
        legend.position="bottom",
        legend.text = element_text(size=16))
dev.off()

# top companies in naic code 511210 (software publishers)
# side by side bar chart of top companies over time (articles with new prods)
# search for company names in tags
techtags$value2<-ifelse(is.na(techtags$value),techtags$value,tolower(techtags$value))

for (i in 1:length(compNaic$company_name_short)) {
  # find company names in tags
  tmp<-techtags[str_detect(techtags$value,paste0("\\b",compNaic$company_name_short[i],"\\b")),] %>%
    filter(.,!is.na(id))
  
  if (nrow(tmp)>0) {
    tmpId<-tmp$id
    tmpComp<-compNaic$company_name_short[i]
    if (i==1) {
      articlesId<-tmpId
      articlesComp<-tmpComp
      df<-data.frame(articlesId,articlesComp)
    } else {
      articlesId<-c(articlesId, tmp)
      articlesComp<-c(articlesComp, tmp)
      df_tmp<-data.frame(articlesId,articlesComp)
      df<-rbind(df,df_tmp)
    }
  }
  
}

naic511210<-filter(techtags,naicComp511210==TRUE)
naic511210_summ<-group_by(naic511210,value) %>%
  dplyr::summarise(count=n())
naic511210_summ<-arrange(naic511210_summ,desc(count))[1:9,1]
naic511210<-filter(naic511210,value %in% top_comp_summ$value)
naic511210<-group_by(naic511210,value,year,new_prod) %>%
  dplyr::summarise(count=n())
naic511210$value<-factor(naic511210$value,levels=(naic511210_summ$value))
naic511210$new_prod2<-ifelse(naic511210$new_prod==1,"New Product Article","Other Article")
naic511210$new_prod2<-factor(naic511210$new_prod2,levels=c("Other Article","New Product Article"))

position = position_stack(vjust = 1)
label <- group_by(naic511210,value,year) %>%
  dplyr::summarise(total=sum(count))
naic511210 <- left_join(naic511210,label,by=c("value","year"))

pdf("top_companies.pdf",width=16,height=8)
ggplot(naic511210) +
  geom_bar(aes(x=year,y=count,fill=new_prod2,group=new_prod2),stat="identity",position=position) +
  geom_text(aes(x=year,y=count,group = new_prod2,label=ifelse(new_prod==0,total,"")),size=5,vjust=0,position = position) +
  facet_wrap(~value,ncol=3) +
  scale_y_continuous(limits=c(0,1050))+
  theme_bw() +
  scale_fill_manual(values=pal,name = "",labels = c("All Other Articles", "New Product Articles")) +
  labs(x="Year",y="Number of Articles") +
  theme(strip.text = element_text(size=18),
        axis.title=element_text(size=18),
        axis.text=element_text(size=16),
        title=element_text(size=20),
        legend.position="bottom",
        legend.text = element_text(size=16))
dev.off()


# world clous of words in title -- new prod articles vs. other articles
# we need to create a corpus.
# articles with new prods
txt<-sample_n(technp, 10000)
#txt<-technp
txt_new<-filter(txt,new_prod==1)
#txt_new<-txt_new$title
txt_new<-txt_new$text
txt_new = unlist(strsplit(unlist(txt_new), "\\s+"))
# other articles
txt_oth<-filter(txt,new_prod==0)
#txt_oth<-txt_oth$title
txt_oth<-txt_oth$text
txt_oth = unlist(strsplit(unlist(txt_oth), "\\s+"))
#test <- iconv(test, to = "utf-8", sub="")
titleCorpus_new <- Corpus(VectorSource(txt_new))
titleCorpus_oth <- Corpus(VectorSource(txt_oth))
# make everything lower case
titleCorpus_new <- tm_map(titleCorpus_new, content_transformer(tolower))
titleCorpus_oth <- tm_map(titleCorpus_oth, content_transformer(tolower))
#convert the corpus to a plain text document.
titleCorpus_new <- tm_map(titleCorpus_new, PlainTextDocument)
titleCorpus_oth <- tm_map(titleCorpus_oth, PlainTextDocument)
#remove all punctuation and stopwords. Stopwords are commonly used words in the English language such as I, me, my, etc. 
# You can see the full list of stopwords using stopwords('english').
titleCorpus_new <- tm_map(titleCorpus_new, removePunctuation)
titleCorpus_new <- tm_map(titleCorpus_new, removeWords, stopwords('english'))
titleCorpus_oth <- tm_map(titleCorpus_oth, removePunctuation)
titleCorpus_oth <- tm_map(titleCorpus_oth, removeWords, stopwords('english'))

# we will perform stemming. This means that all the words are converted to their stem 
# (Ex: learning -> learn, walked -> walk, etc.). This will ensure that different forms of the word are converted 
# to the same form and plotted only once in the wordcloud.
titleCorpus_new <- tm_map(titleCorpus_new, stemDocument)
titleCorpus_oth <- tm_map(titleCorpus_oth, stemDocument)

# If you want to remove the words ‘the’ and ‘this’, you can include them in the removeWords function as follows:
titleCorpus_new <- tm_map(titleCorpus_new, removeWords, c('the', 'this', 'launch',stopwords('english')))
titleCorpus_oth <- tm_map(titleCorpus_oth, removeWords, c('the', 'this', 'launch',stopwords('english')))
#titleCorpus[[1]]$content
titleCorpus_new <- tm_map(titleCorpus_new, stripWhitespace)
titleCorpus_oth <- tm_map(titleCorpus_oth, stripWhitespace)

titleCorpus_new <- Corpus(VectorSource(titleCorpus_new))
titleCorpus_oth <- Corpus(VectorSource(titleCorpus_oth))

# plot wordclouds
pdf("wordclouds_text.pdf",width=12,height=6)
nf<-layout(matrix(c(1, 2, 3, 4), nrow=2), heights=c(0, 4))
par(mfrow=c(1,2),mfcol=c(1,2))

layout(matrix(c(1,2,3,4), 2, 2, byrow = TRUE), respect = TRUE)
#layout.show(nf)
par(mfrow=c(1,2),mfcol=c(1,2))

plot.new()
text(x=0.5, y=0.5, "Title of my first plot")
text(x=1, y=0.5, "Title of my second plot")
#par(mfrow=c(1,2))
wordcloud(titleCorpus_new, max.words = 500, random.order = FALSE)
wordcloud(titleCorpus_oth, max.words = 500, random.order = FALSE)
dev.off()

## wordclouds of text of articles

# is part of the goal to automatically find articles related to new products? 
# learn which articles are about new products (training set would be tech articles that are about new products), 
# apply neural net across all articles

# can we figure out the product the article is talking about??
##### remove all words in english dictinary, and company names -- what is left is product name?
# number of articles per day, what does tech crunch do? (about techcrunch)
# how did I get the data? describe data we got
# naic code -- 511210 (software publishes -- 324 companies??) (Microsoft and others)
# get count of articles for all companies in this naic code
# do google and facebook naics codes next (they have the same naics code - 518210)

# think more about keywords to use

# tfidf -- down weights words that appear everywhere and weights unique words



# find company names in article text



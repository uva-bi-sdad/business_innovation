NYTIMES_KEY="gnTtYZLmWuoG46J43AxpM4I7mpqiItmy"


install.packages("devtools")
devtools::install_github("mkearney/nytimes")

install.packages("jsonlite")
library(jsonlite)
library(dplyr)
library(ggplot2)

x <- fromJSON("http://api.nytimes.com/svc/search/v2/articlesearch.json?q=mueller&api-key=gnTtYZLmWuoG46J43AxpM4I7mpqiItmy")
x <- fromJSON("http://api.nytimes.com/svc/search/v2/articlesearch.json?q=mueller&api-key=gnTtYZLmWuoG46J43AxpM4I7mpqiItmy", flatten = TRUE) %>% data.frame()

list(name = c("persons", "organizations", "subject"),
     value = c("Mueller, Robert S III", "Federal Bureau of Investigation", "United States Politics and Government"),
     rank = 1:3,
     major = c("N", "N", "N"))

# Let's set some parameters
term <- "new+product" # Need to use + to string together separate words
begin_date <- "20160101"
end_date <- "20160115"

baseurl <- paste0("http://api.nytimes.com/svc/search/v2/articlesearch.json?q=",term,
                  "&begin_date=",begin_date,"&end_date=",end_date,
                  "&facet_filter=true&api-key=",NYTIMES_KEY, sep="")

initialQuery <- fromJSON(baseurl)
maxPages <- round((initialQuery$response$meta$hits[1] / 10)-1)

pages <- list()
for(i in 0:maxPages){
  nytSearch <- fromJSON(paste0(baseurl, "&page=", i), flatten = TRUE) %>% data.frame()
  message("Retrieving page ", i)
  pages[[i+1]] <- nytSearch
  Sys.sleep(1)
}

allNYTSearch <- rbind_pages(pages)

# Visualize coverage by section
allNYTSearch %>%
  group_by(response.docs.type_of_material) %>%
  summarize(count=n()) %>%
  mutate(percent = (count / sum(count))*100) %>%
  ggplot() +
  geom_bar(aes(y=percent, x=response.docs.type_of_material, fill=response.docs.type_of_material), stat = "identity") + coord_flip()

allNYTSearch %>%
  mutate(pubDay=gsub("T.*","",response.docs.pub_date)) %>%
  group_by(pubDay) %>%
  summarise(count=n()) %>%
  #filter(count >= 2) %>%
  ggplot() +
  geom_bar(aes(x=reorder(pubDay, count), y=count), stat="identity") + coord_flip()

colnames(allNYTSearch)
head(allNYTSearch)

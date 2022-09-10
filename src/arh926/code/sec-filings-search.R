# source('~/Desktop/SEC-filings/code/strsplit_sec.R')
# Run-First Time
# all_filings <- read.csv(file.list[i])[,-1]
# filings_list <- list()
# for(k in 1:length(all_filings)){
#   filings_list[[k]] <- strsplit_sec(as.character(all_filings[k]))
# }
require(tokenizers)
require(qdap)
require(qdapDictionaries)
require(stringdist)
require(stringr)
data("GradyAugmented")
sic_codes <- readxl::read_xlsx("/Users/aritrah/Desktop/FINAL-naics_SIC codes(1).xlsx",sheet = 8)
food_sic <- sic_codes[23:40,]
load("/Users/aritrah/Downloads/SEC-filings-FoodBev.RData")

setwd("/Users/aritrah/Desktop/SEC-filings/sec filings txt/")
file.list <- list.files()
count <- c()
for(i in 1:length(file.list)){
  file.info <- strsplit(file.list[i],split = "-")
  food_company <- file.info[[1]][1]; file.year <- file.info[[1]][4]
  file_txt <- paste0(readLines(file.list[i]), collapse=" ")
  file_txt_words <- tokenize_words(file_txt)[[1]]
  # Sentiment Analysis of Filing
  file_txt_unq.names <- table(file_txt_words)
  file_txt_unq.names <- file_txt_unq.names[!is.na(match(names(file_txt_unq.names),GradyAugmented))]
  barplot(file_txt_unq.names[file_txt_unq.names<5],las=2,horiz = T,cex.names = 0.5)
  
  file_txt_sentences <- tokenize_sentences(file_txt)[[1]]
  id_reg <- file_txt_sentences[unlist(lapply(str_locate_all(pattern ='&#174', file_txt_sentences), nrow))>0]
  id_trade <- file_txt_sentences[unlist(lapply(str_locate_all(pattern ='&#8482', file_txt_sentences), nrow))>0]
  id_launch <- file_txt_sentences[unlist(lapply(str_locate_all(pattern ='launch', file_txt_sentences), nrow))>0]
  id_trade1 <- file_txt_sentences[unlist(lapply(str_locate_all(pattern ='trademark', file_txt_sentences), nrow))>0]
  
  count <- rbind(count,c(file.year,
                         food_company,
                         length(id_reg), # registered symbol
                         length(id_trade),# trademark symbol 
                         length(id_launch),
                         length(id_trade1),
                         paste0(id_reg,collapse=";\t"),
                         paste0(id_trade,collapse=";\t"),
                         paste0(id_launch,collapse=";\t"),
                         paste0(id_trade1,collapse=";\t"))) 
  
  #if(length(id_reg)>0 & length(id_trade)==0){
  #  phrase[[i]] <- list(year=file.year,
  #                      company=food_company,
  #                      reg_symb=filings_list[id_reg],
  #                      trade_symb=0)
  #}else if(length(id_reg)==0 & length(id_trade)>0){
  #  phrase[[i]] <- list(year=file.year,
  #                      company=food_company,
  #                      reg_symb=0,
  #                      trade_symb=filings_list[id_trade])
  #}else if(length(id_reg)>0 & length(id_trade)>0){
  #  phrase[[i]] <- list(year=file.year,
  #                      company=food_company,
  #                      reg_symb=filings_list[id_reg],
  #                      trade_symb=filings_list[id_trade])
  #}else{
   # phrase[[i]] <- 0
  #}
  cat(i,"\t",food_company,"\t",file.year,"\n")
}
company_names <- table(count[,2])
food_bev_counts  <- matrix(0, ncol=18, nrow=length(company_names))
rownames(food_bev_counts) <- names(company_names)
colnames(food_bev_counts) <- paste(c("rgstrd.","trdmk."),
                                   rep(2013:2021,each=2),
                                   sep="-")

for(i in 1:nrow(count)){
  row.id <- match(count[i,2],rownames(food_bev_counts))
  col.id <- match(count[i,1],2013:2021)
  food_bev_counts[row.id,c(2*col.id-1,2*col.id)] <- count[i,c(3:4)]
}
food_bev_counts <- data.frame(food_bev_counts)
write.csv(food_bev_counts, file="/Users/aritrah/Desktop/FoodBevCounts.csv")

write.csv(count, file="/Users/aritrah/Desktop/FoodBevCounts-raw.csv")
# Storing clean filings (311 and 312)
#file.list <- list.files()
#for(i in 1:length(file.list)){
#  file.info <- strsplit(file.list[i],split = ".csv")
#  food_company <- file.info[[1]][1]
#  clean.file.name <- paste(file.info,".txt", sep="")
#  all_filings <- read.csv(file.list[i])[,-1]
#  filings_list <- list()
#  for(k in 1:length(all_filings)){
#    filings_list[[k]] <- strsplit_sec(as.character(all_filings[k]))
#  }
#  clean_filings <- unlist(filings_list)
#  writeLines(clean_filings,clean.file.name)
#  cat(i,"\t",food_company,"\n")
#}

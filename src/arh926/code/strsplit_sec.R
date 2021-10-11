require(stringr)
## Run First
strsplit_sec <- function(x){
  ncharx <- nchar(as.character(x))
  start.char <- str_locate_all(pattern ='<', x)
  end.char <- str_locate_all(pattern ='>', x)
  if(is.na(ncharx)) return("")
  else if(nrow(end.char[[1]])==0 & nrow(start.char[[1]])==0){
    str_check <-  str_locate_all(pattern ='Times New Roman', x)
    if(nrow(str_check[[1]])>1) return("")
    else return(x)
  } 
  else if(nrow(end.char[[1]])==1 & nrow(start.char[[1]])==1){
    if((end.char[[1]]-start.char[[1]])[1]==3){
      str_final <- substr(x,1,start.char[[1]][1,1]-1)
      if(str_final=="") return("")
      else return(str_final)
    }else return("")
  }
  else if(nrow(end.char[[1]])>nrow(start.char[[1]])|nrow(end.char[[1]])<nrow(start.char[[1]])) return("")
  else if(start.char[[1]][1,1]==1 & end.char[[1]][nrow(end.char[[1]]),2]==ncharx) return("")
  else{
    if(start.char[[1]][1,1]!=1 & end.char[[1]][nrow(end.char[[1]]),2]==ncharx){
      str_final <- substr(x,1,start.char[[1]][1,1]-1)
      if(str_final=="") return("")
      else return(str_final)
    }else{
      str_final <- substr(x,end.char[[1]][nrow(end.char[[1]]),2]+1,ncharx)
      if(str_final=="") return("")
      else return(str_final)
    }
  }
}

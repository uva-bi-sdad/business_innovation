#function for quickly cleaning company names
cleaning <- function(name_list){
  clean_name <- tolower(name_list)
  clean_name <- str_trim(clean_name, side = 'both') %>%
    str_replace_all(" \\+ ", " and ") %>%
    str_replace_all("\\,.*", "") %>%
    str_replace_all(": ", "") %>%
    str_replace_all("\\s\\(.*", "") %>%
    str_replace_all("\\;.*", "") %>%
    # str_replace_all("[[:punct:]]", "") %>%
    str_replace_all(" inc$","") %>%
    str_replace_all(" inc $","") %>% #result of taking out (
    str_replace_all(" pharm.*$","") %>%
    str_replace_all(" technologies$","") %>%
    str_replace_all(" intl$", "") %>%
    str_replace_all(" llc$", "") %>%
    str_replace_all(" & co$", "") %>%
    str_replace_all(" co$", "") %>%
    str_replace_all(" north america$", "") %>%
    str_replace_all(" international$", "") %>%
    str_replace_all(" labs$", "") %>%
    str_replace_all(" brands.*$", "") %>%
    # str_replace_all(" products$", "") %>%
    str_replace_all("sanofiaventis", "sanofi aventis") %>%
    str_replace_all(" us$", "") %>%
    str_replace_all(" usa$", "") %>%
    str_replace_all(" corp.*$", "") %>%
    str_replace_all(" industries$", "") %>%
    str_replace_all(" electronics$", "") %>%
    str_replace_all(" medical$", "") %>%
    str_replace_all(" cons ", " consumer ") %>%
    str_replace_all(" cons$", " consumer")%>%
    str_replace_all(" con ", " consumer ")%>%
    str_replace_all(" con$", " consumer")%>%
    str_replace_all(" consumer.*$", "")%>%
    str_replace_all("hlth", "health") %>%
    str_replace_all("health care", "healthcare") %>%
    str_replace_all("glaxosmithkline consumer [^h]", "glaxosmithkline consumer healthcare") %>%
    str_replace_all("philadelphia pa", "") %>%
    str_replace_all("philadelphia", "") %>%
    str_replace_all("pittsburgh", "") %>%
    str_replace_all("glaxosmithkline consumer [^h]", "glaxosmithkline consumer healthcare") %>%
    str_replace_all("philadelphia", "") %>%
    str_replace_all("philadelphia pa", "") %>%
    str_replace_all(" pa(\\s|$)", "") %>%
    str_replace_all(" england", "") %>%
    str_replace_all(" grp ltd", " grp") %>%
    str_replace_all(" denver", "") %>%
    str_replace_all("(?= health(\\s?)care)(.*)", "") %>%
    str_trim(side = 'both') %>%
    str_replace_all(" consumer healthcare.*$", " consumer healthcare") %>%
    str_replace_all(" ireland$", "") %>%
    str_replace_all("johnsonmerck", "johnson-merck") %>%
    str_replace_all("johnson johnson ", "johnson and johnson ") %>%
    str_replace_all("johnson  johnson ", "johnson and johnson ") %>%
    str_replace_all("j and j", "johnson and johnson") %>%
    str_replace_all("johnson and johnson consumer$", "johnson and johnson consumer healthcare") %>%
    str_replace_all("johnson and johnsonconsumer healthcare", "johnson and johnson consumer healthcare") %>%
    str_replace_all("(?= health(\\s?)care)(.*)", " healthcare") %>%
    str_replace_all(" and$", "")%>%
    str_replace_all(" bristolmyers squibb", "bristol myers squibb") %>%
    str_replace_all("sigmatau", "sigma tau") %>%
    str_replace_all(" ip$", "") %>%
    str_replace_all(" laboratories$", "")
  return(clean_name)
}

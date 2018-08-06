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
    str_replace_all(" laboratories$", "") %>%
    str_replace_all("^lilly$", "eli lilly") %>%
    str_replace_all("^3m.*$", "3m") %>%
    str_replace_all("^abbvie.*$", "abbvie") %>%
    str_replace_all("^actavis.*$", "actavis")%>%
    str_replace_all("^air liquid.*$", "air liquid")%>%
    str_replace_all("^airgas.*$", "airgas")%>%
    str_replace_all("^alkem.*$", "alkem")%>%
    str_replace_all("^alergan.*$", "allergan")%>%
    str_replace_all("^alvogen.*$", "alvogen")%>%
    str_replace_all("^astrazeneca.*$", "astrazeneca")%>%
    str_replace_all("^bristol myers", "bristol-myers")%>%
    str_replace_all("^dr reddys.*$", "dr reddys")%>%
    str_replace_all("^fresenius.*$", "fresenius")%>%
    str_replace_all("^galderma.*$", "galderma")%>%
    str_replace_all("^hetero .*", "hetero") %>%
    str_replace_all("^hoffman la", "hoffman-la")%>%
    str_replace_all("^horizon.*", "horizon")%>%
    str_replace_all("^ipsen.*$", "ipsen") %>%
    str_replace_all("^janssen.*$", "janssen")%>%
    str_replace_all("^linde.*$", "linde")%>%
    str_replace_all("^mallinkrodt.*$", "mallinkrodt")%>%
    str_replace_all("^mylan.*$", "mylan")%>%
    str_replace_all("l perrigo", "perrigo")%>%
    str_replace_all("^perrigo.*$", "perrigo")%>%
    str_replace_all("^praxair.*", "praxair")%>%
    str_replace_all("^ranbaxy.*$", "ranbaxy")%>%
    str_replace_all("^sagent.*$", "sagent")%>%
    str_replace_all("sanofi aventis", "sanofi-aventis")%>%
    str_replace_all("sanofi us services", "sanofi")%>%
    str_replace_all(" holding$", "")%>%
    str_replace_all("^shire.*", "shire")%>%
    str_replace_all("^teva.*$", "teva")%>%
    str_replace_all("^unichem.*$", "unichem")%>%
    str_replace_all("^valeant.*$", "valeant")%>%
    str_replace_all("^wockhardt.*$", "wockhardt")%>%
    str_replace_all("^zydus.*$", "zydus")
  return(clean_name)
}

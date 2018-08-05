# product overlap




#check how many products are in FDA and pharmacy times
FDA$Drug.Name <- str_replace(FDA$Drug.Name, " \\#.*$","")
semi_join(pharmtimes,FDA,by='Drug Name')
#no products match... let's check company matches
x <- semi_join(pharmtimes,FDA,by='Company')
#unique
length(unique(x$Company))


semi_join(ptoday,FDA,by='Drug.Name')
#no products match... let's check company matches
x <- semi_join(ptoday,FDA,by='Company')
#unique
length(unique(x$Company))

#Code to format all the Pharmacy Times text files into one single data frame
library(RCurl)
library(xml2)
library(stringr)
library(rvest)
library(jsonlite)

#load the first text file as an example
dat <- readLines("./pharmacyTimes_OTC/1PTscrape_NOVEMBER 16, 2017.txt", n=3)
dat <- as.data.frame(t(dat))

names(dat) = c("Product", "Company", "Description", "Link")

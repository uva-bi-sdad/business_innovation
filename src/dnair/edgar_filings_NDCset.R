---
title: "Untitled"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)

## LIBRARIES
R.utils::sourceDirectory("functions")
library(xml2)
library(rvest)
library(stringr)
library(hunspell)
library(data.table)
library(dplyr)
library(htmltools)
library(magrittr)
library(htmltidy)
library(readr)

```

```{r}
project_folder <- "/project/biocomplexity/sdad/projects_data/ncses/bi/binn/"
```


```{r }
##GRAB METADATA
cik_ticker <- read_delim(paste0(project_folder, "original/edgar_filings/cik_ticker.csv"), "|", escape_double = FALSE, trim_ws = TRUE)
head(cik_ticker)

```

```{r}
ndc_sec_ref <- readxl::read_excel(paste0(project_folder, "working/NDC_SEC_CompNames/final_ndc_sec_companies.xlsx"), sheet = "finalset") %>% select(-12,-13, -14)
colnames(ndc_sec_ref) <- dataplumbr::name.standard_col_names(colnames(ndc_sec_ref))
ndc_sec_ref <- ndc_sec_ref %>% filter(is.na(dupe)) %>% select(-dupe)

refciks <- unique(ndc_sec_ref$cik)
```


```{r}
## GRAB ALL PATHS
paths_file <- paste0(project_folder, "original/edgar_filings/ALL_SEC_files.txt")
file_headers <- readr::read_tsv(paths_file, col_names = FALSE)

patt <- paste0("^(", paste0(refciks, collapse = "|"), ")" )

ndc_file_headers <- file_headers[str_detect(file_headers$X1, pattern = patt),]

paths <- paste0(project_folder, "original/edgar_filings/Edgar_filings_folders/", file_headers$X1)
paths[9]
file_names <- unique(list.files(paths, full.names = TRUE))
file_names[9]
head(file_names)
```













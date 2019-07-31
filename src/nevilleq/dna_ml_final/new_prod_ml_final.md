Final New Product ML
================
Quinton Neville
7/31/2019

1. Read, Tidy, and Format the Data
==================================

``` r
#Store desired filepath
file.path <- "./data/working/DNA_Aggregated/Machine_learning_sample/NPS_sample_data/"

#Test train sample
train.split <- 0.8
N <- 1000
set.seed(2019)
train.samp <- sample(1:N, train.split * N, replace = FALSE)

#Create tibble of each sample data identified by the variable 'sample', stored in the variable 'data'
sample.df <- list.files(path = file.path) %>%
  enframe() %>%
  rename(file_path = value) %>%
  filter(file_path %>% str_detect(".RDS")) %>%
  mutate(
    sample      = str_split_fixed(file_path, "_", 5)[ ,4],
    file_path   = str_c(file.path, file_path),
    data        = map(.x = file_path, ~read_rds(.x) %>% as_tibble()),
    subject     = map(.x = data, ~.x %>% dplyr::select(subject_code_ns) %>% 
                                  mutate(subject_code_ns = as.factor(subject_code_ns))),
    data        = map2(.x = data, .y = subject, ~bind_cols(as_tibble(make_dtm(.x, 40)), .y) %>%
                                                 dplyr::select(subject_code_ns, everything())),
    train       = map(.x = data, ~.x[train.samp, ]),
    test        = map(.x = data, ~.x[setdiff(1:N, train.samp), ])
  ) %>%
  dplyr::select(-file_path)
```

New Product/Service ML Methods
================
DSPG Business Innovation Team
7/23/2019

1. Read in NPS Sample Data (50%, 20%, 10%, Proportional).
---------------------------------------------------------

Here, we read in the data files each containing 1000 randomly sampled articles from DNA (2015); with 50%, 20%, 10%, and Proportional (to empirical distribution) describing articles coded as a New Product or Service and the remainder coming from elsewhere. The outcome of interest in our modeling is whether or not an article was labeled as a New Product or Service or not

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
  mutate(
    sample      = str_split_fixed(file_path, "_", 5)[ ,4],
    file_path   = str_c(file.path, file_path),
    data        = map(.x = file_path, ~read_rds(.x) %>% as_tibble()),
    subject     = map(.x = data, ~.x %>% dplyr::select(subject_code_ns) %>% 
                                  mutate(subject_code_ns = as.factor(subject_code_ns))),
    data        = map2(.x = data, .y = subject, ~bind_cols(make_dtm(.x, 40), .y) %>%
                                                 dplyr::select(subject_code_ns, everything())),
    train       = map(.x = data, ~.x[train.samp, ]),
    test        = map(.x = data, ~.x[setdiff(1:N, train.samp), ])
  ) %>%
  dplyr::select(-file_path)
```

2. Machine Learning Methods
---------------------------

We investigated a variety of machine learning methods, supervised and unsupervised, to explore the data and generate predictive models to classify an article from a given DNA (2015) random sample by New Product/Service outcome %; as a New Product/Service or not (binary). We will further assess these models according to diagnostics given by A

### i. Supervised

#### a. Penalized Logistic Regression

Generalized linear models are one of the simplest and easily understood machine learning techniques, relying on parametric probability distributions and Newton Raphson or Coordinate Descent maximum likelihood algorithms. In this case, we employ a Logistic Regression to model our document term matrices by whether or not a new product or service was labeled for each observation (coming from a Bernoulli/Binomial distribution). In early iterations, this modeling was performing the best (at the 50-50 New Product Split), implying that there may exist some underlying linear structure to the DNA New Product labeling. Here we fit a generalized penalized logistic regression to each training sample, optimize the *α* (geometric penalization type) and *λ* (penalization/shrinkage) parameters, and predict the probability that a given article in each test sample describes a New Product or Service, or not.

Results below give a table of the optimized parameter values, as well as the ROC and

``` r
#Penalized Logistic Regression over 
glm.df <- sample.df %>%
  filter(sample == "half" | sample == "prop") %>%
  mutate(
    glm = map(.x = train, ~train(subject_code_ns ~ ., data = .x, method = "glmnet",
                 family = "binomial", trControl = trainControl(method = "cv", number = 10))),
    glm_preds = map2(.x = glm, .y = test, ~predict(.x, newdata = .y)),
    glm_probs = map2(.x = glm, .y = test, ~predict(.x, newdata = .y, type = "prob")[ ,2]),
    alpha     = map_dbl(.x  = glm, ~.x$bestTune[, 1]),
    lambda    = map_dbl(.x = glm,  ~.x$bestTune[, 2])
  )

#Check out best tuning parameters
glm.df %>%
  select(sample, alpha, lambda) %>%
  knitr::kable(digits = 5)
```

| sample |  alpha|   lambda|
|:-------|------:|--------:|
| half   |   0.10|  0.09651|
| prop   |   0.55|  0.04667|

``` r
#Plot Roc
plots <- glm.df %>%
  mutate(
    roc     = map2(.x = test, .y = glm_probs, ~roc.log(.x, .y)),
    roc_gg  = map2(.x = roc, .y  = c("Fifty-Fifty", "Proportional"), 
                  ~plot.roc(.x, sprintf("GLM %s | AUC: %s", .y, auc(.x) %>% round(4)*100))),
    cost    = map2(.x = test, .y = glm_probs, ~cost.df(.x, .y)),
    cost_gg = map(.x = cost, .y = c("Fifty-Fifty", "Proportional"), 
                  ~plot.cost(.x, paste("GLM", .y)))
  )

#Store GG panels
glm.roc  <- plots$roc_gg[[1]] + plots$roc_gg[[2]]
glm.cost <- plots$cost_gg[[1]] + plots$cost_gg[[2]]

#Save
ggsave("./src/nevilleq/dna_sample_predictive_modeling/new_prod_ml_figures/glm_roc.jpg" , glm.roc)
```

    ## Saving 8 x 5 in image

``` r
ggsave("./src/nevilleq/dna_sample_predictive_modeling/new_prod_ml_figures/glm_cost.jpg", glm.cost, width = 8)
```

    ## Saving 8 x 5 in image

``` r
#Display
glm.roc
```

<img src="newprod_ml_methods_files/figure-markdown_github/glm-1.png" width="90%" />

``` r
glm.cost
```

<img src="newprod_ml_methods_files/figure-markdown_github/glm-2.png" width="90%" />

#### b. Random Forest

``` r
#Random Forest 
rf.df <- sample.df %>%
  filter(sample == "half" | sample == "prop") %>%
  mutate(
    rf        = map(.x = train, ~train(subject_code_ns ~ ., data = .x,
                                       method = "rf", family = "bernoulli",
                                       trControl = trainControl(method = "cv", number = 5))),
    rf_preds = map2(.x = rf, .y = test, ~predict(.x, newdata = .y)),
    rf_probs = map2(.x = rf, .y = test, ~predict(.x, newdata = .y, type = "prob")[ ,2]),
    n_trees  = map_dbl(.x = rf, ~.x$finalModel$ntree),
    mtry     = map_dbl(.x = rf, ~.x$finalModel$mtry)
  )



#Check out best tuning parameters
rf.df %>%
  select(sample, n_trees, mtry) %>%
  knitr::kable(digits = 5)
```

| sample |  n\_trees|  mtry|
|:-------|---------:|-----:|
| half   |       500|    49|
| prop   |       500|     2|

``` r
#Plot Roc
plots <- rf.df %>%
  mutate(
    roc     = map2(.x = test, .y = rf_probs, ~roc.log(.x, .y)),
    roc_gg  = map2(.x = roc, .y  = c("Fifty-Fifty", "Proportional"), 
                  ~plot.roc(.x, sprintf("RF %s | AUC: %s", .y, auc(.x) %>% round(4)*100))),
    cost    = map2(.x = test, .y = rf_probs, ~cost.df(.x, .y)),
    cost_gg = map(.x = cost, .y = c("Fifty-Fifty", "Proportional"), 
                  ~plot.cost(.x, paste("RF", .y)))
  )

#Store GG panels
rf.roc  <- plots$roc_gg[[1]] + plots$roc_gg[[2]]
rf.cost <- plots$cost_gg[[1]] + plots$cost_gg[[2]]

#Save
ggsave("./src/nevilleq/dna_sample_predictive_modeling/new_prod_ml_figures/rf_roc.jpg", rf.roc)
```

    ## Saving 8 x 5 in image

``` r
ggsave("./src/nevilleq/dna_sample_predictive_modeling/new_prod_ml_figures/rf_cost.jpg", rf.cost, width = 8)
```

    ## Saving 8 x 5 in image

``` r
#Display
rf.roc
```

<img src="newprod_ml_methods_files/figure-markdown_github/rf-1.png" width="90%" />

``` r
rf.cost
```

<img src="newprod_ml_methods_files/figure-markdown_github/rf-2.png" width="90%" />

#### c. Boosting (GBM Trees)

``` r
#Boosting (GBM Trees)
gbm.df <- sample.df %>%
  filter(sample == "half" | sample == "prop") %>%
  mutate(
    gbm        = map(.x = train, ~train(subject_code_ns ~ ., data = .x, 
                                       distribution = "bernoulli", method = "gbm",
                                       trControl = trainControl(method = "cv", number = 5))),
    gbm_preds = map2(.x = gbm, .y = test, ~predict(.x, newdata = .y)),
    gbm_probs = map2(.x = gbm, .y = test, ~predict(.x, newdata = .y, type = "prob")[ ,2]),
    n_trees   = map_dbl(.x = gbm, ~.x$bestTune[, 1]),
    interaction_depth = map_dbl(.x = gbm,  ~.x$bestTune[, 2]),
    shrinkage = map_dbl(.x = gbm,  ~.x$bestTune[, 3]),
    n_bins    = map_dbl(.x = gbm,  ~.x$bestTune[, 4])
  )
```

``` r
#Check out best tuning parameters
gbm.df %>%
  select(sample, n_trees, interaction_depth, shrinkage, n_bins) %>%
  knitr::kable(digits = 5)
```

| sample |  n\_trees|  interaction\_depth|  shrinkage|  n\_bins|
|:-------|---------:|-------------------:|----------:|--------:|
| half   |       100|                   3|        0.1|       10|
| prop   |        50|                   2|        0.1|       10|

``` r
#Plot Roc
plots <- gbm.df %>%
  mutate(
    roc     = map2(.x = test, .y = gbm_probs, ~roc.log(.x, .y)),
    roc_gg  = map2(.x = roc, .y  = c("Fifty-Fifty", "Proportional"), 
                  ~plot.roc(.x, sprintf("GBM %s | AUC: %s", .y, auc(.x) %>% round(4)*100))),
    cost    = map2(.x = test, .y = gbm_probs, ~cost.df(.x, .y)),
    cost_gg = map(.x = cost, .y = c("Fifty-Fifty", "Proportional"), 
                  ~plot.cost(.x, paste("GBM", .y)))
  )

#Store GG panels
gbm.roc  <- plots$roc_gg[[1]] + plots$roc_gg[[2]]
gbm.cost <- plots$cost_gg[[1]] + plots$cost_gg[[2]]

#Save
ggsave("./src/nevilleq/dna_sample_predictive_modeling/new_prod_ml_figures/gbm_roc.jpg", gbm.roc)
```

    ## Saving 8 x 5 in image

``` r
ggsave("./src/nevilleq/dna_sample_predictive_modeling/new_prod_ml_figures/gbm_cost.jpg", gbm.cost, width = 8)
```

    ## Saving 8 x 5 in image

``` r
#Display
gbm.roc
```

<img src="newprod_ml_methods_files/figure-markdown_github/gbm_plots-1.png" width="90%" />

``` r
gbm.cost
```

<img src="newprod_ml_methods_files/figure-markdown_github/gbm_plots-2.png" width="90%" />

### ii. Unsupervised

#### a. K-means

#### b. Heirarchical

#### c. Bayesian

#### d. Latent Dirichlet Allocation

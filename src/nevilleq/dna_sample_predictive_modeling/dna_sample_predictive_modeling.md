DNA Sample NS Prediction
================
DSPG Business Innovation Team
7/15/2019

1. Read and Split Sample
------------------------

Read, clean, and tidy; just like it's Sunday.

``` r
#Read in the data
dna.df <- read_rds("./data/working/DNA_Aggregated/Machine_learning_sample/7_15_19_sample_data.RDS") %>%
  slice(-c(which(body == "")))

#Transfrom Body Text into a corpus and clean
dna.corpus <- Corpus(VectorSource(dna.df$body)) %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(removeWords, c("the", "and", stopwords("english"))) %>%
  tm_map(stripWhitespace)
```

    ## Warning in tm_map.SimpleCorpus(., content_transformer(tolower)):
    ## transformation drops documents

    ## Warning in tm_map.SimpleCorpus(., removeNumbers): transformation drops
    ## documents

    ## Warning in tm_map.SimpleCorpus(., removePunctuation): transformation drops
    ## documents

    ## Warning in tm_map.SimpleCorpus(., removeWords, c("the", "and",
    ## stopwords("english"))): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(., stripWhitespace): transformation drops
    ## documents

``` r
#Document Term Matrix
dna.dtm.df = DocumentTermMatrix(dna.corpus)
#dna.dtm.df = docTfidf
str(dna.dtm.df)
```

    ## List of 6
    ##  $ i       : int [1:143706] 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ j       : int [1:143706] 1 2 3 4 5 6 7 8 9 10 ...
    ##  $ v       : num [1:143706] 1 1 1 2 1 1 1 1 2 2 ...
    ##  $ nrow    : int 999
    ##  $ ncol    : int 24738
    ##  $ dimnames:List of 2
    ##   ..$ Docs : chr [1:999] "1" "2" "3" "4" ...
    ##   ..$ Terms: chr [1:24738] "alfred" "allstock" "andrémichel" "announced" ...
    ##  - attr(*, "class")= chr [1:2] "DocumentTermMatrix" "simple_triplet_matrix"
    ##  - attr(*, "weighting")= chr [1:2] "term frequency" "tf"

``` r
#Outcome
new.product <- dna.df$subject_code_ns

#Add Outcome to Document Matrix
dna.doc.dt = data.table(doc = dna.dtm.df$i, word = dna.dtm.df$dimnames$Terms[dna.dtm.df$j], count = dna.dtm.df$v) %>%
  merge(data.table(doc = seq_along(new.product), new_product = new.product))


# This gives how many words appear a given number of times in total
wordTable <- dna.doc.dt[,.(count = sum(count)), by = c('word')] %>% arrange(-count) %>% data.table
table(wordTable$count)
```

    ## 
    ##     1     2     3     4     5     6     7     8     9    10    11    12 
    ## 11532  3502  1831  1224   855   673   475   378   343   264   255   216 
    ##    13    14    15    16    17    18    19    20    21    22    23    24 
    ##   204   132   144   166   147   116    90    97    77    99    81    56 
    ##    25    26    27    28    29    30    31    32    33    34    35    36 
    ##    62    59    61    53    53    54    49    46    41    36    37    41 
    ##    37    38    39    40    41    42    43    44    45    46    47    48 
    ##    27    31    28    35    20    31    24    14    28    28    28    14 
    ##    49    50    51    52    53    54    55    56    57    58    59    60 
    ##    26    25    20    18    16    17     9    13    20    12    21    11 
    ##    61    62    63    64    65    66    67    68    69    70    71    72 
    ##    12    16    13    13     9    11    13    11    10    14     8    11 
    ##    73    74    75    76    77    78    79    80    81    82    83    84 
    ##     5    11    11     7    11     5     4    14     5     7     6     4 
    ##    85    86    87    88    89    90    91    92    93    94    95    96 
    ##    13    10     6     4    10     4     6     6     5     1     3     3 
    ##    97    98    99   100   101   102   103   104   105   106   107   108 
    ##     5     6     3     6     3     5     5     2     2     3     7     4 
    ##   109   110   111   112   113   114   115   116   117   118   119   121 
    ##     4     5     5     5     4     2     6     3     1     1     5     6 
    ##   122   123   124   125   126   127   128   129   130   131   132   133 
    ##     3     2     2     3     5     2     2     6     3     2     3     2 
    ##   134   135   136   137   138   139   140   141   142   143   144   145 
    ##     2     4     4     1     3     1     2     3     5     2     4     1 
    ##   146   147   148   150   151   153   154   156   157   158   159   160 
    ##     4     4     4     2     2     5     1     1     2     5     1     1 
    ##   161   163   164   165   167   168   169   170   171   173   174   175 
    ##     1     4     1     2     1     4     1     1     1     2     1     1 
    ##   178   179   180   181   182   183   184   187   188   189   190   192 
    ##     1     1     2     1     1     2     3     2     1     1     4     1 
    ##   193   195   196   197   198   200   203   204   205   206   207   208 
    ##     1     2     1     2     1     1     2     1     2     1     1     1 
    ##   210   216   217   218   219   223   224   226   227   229   230   232 
    ##     1     1     2     2     5     2     1     1     2     2     2     1 
    ##   235   238   240   241   243   245   246   249   250   251   253   254 
    ##     1     1     1     2     1     1     1     1     1     1     1     1 
    ##   256   258   263   266   271   272   274   275   277   283   286   287 
    ##     1     3     3     1     2     1     2     1     1     1     1     1 
    ##   296   298   299   300   302   313   314   321   324   327   331   338 
    ##     1     1     1     1     1     1     1     2     2     1     2     1 
    ##   339   342   343   345   346   353   354   373   377   398   402   404 
    ##     1     1     1     1     1     1     1     1     1     2     1     2 
    ##   412   414   416   434   444   452   462   467   481   489   510   519 
    ##     1     1     1     1     1     1     1     1     1     1     1     1 
    ##   527   536   547   549   551   555   578   584   637   684   706   725 
    ##     1     1     1     1     1     1     1     1     1     1     1     1 
    ##   762   773   797   827   872   897   946   986   993  1037  1058  1580 
    ##     1     1     1     1     1     1     1     1     1     1     1     1

``` r
#18722 - sum(table(wordTable$count)[1:20])

# We require a word to appear at least this many times to be included in the analysis
minWordCountThreshold <- 40

survivorWords <- wordTable[count >= minWordCountThreshold, word]

# Now we look at words broken out by topic

reducedWordCount = dna.doc.dt[word %in% survivorWords]
reducedWordCount = data.table(reducedWordCount, filterWordIndex = match(reducedWordCount$word, unique(reducedWordCount$word)))

#
#reducedWordCount = data.table(dna.doc.dt, filterWordIndex = match(dna.doc.dt$word, unique(dna.doc.dt$word)))
#

# Turn into a long format matrix

dna.mat <- matrix(0, nrow = length(dna.corpus), ncol = length(unique(reducedWordCount$word)))
for(i in 1:nrow(reducedWordCount)) dna.mat[reducedWordCount$doc[i], reducedWordCount$filterWordIndex[i]] = reducedWordCount$count[i]
rownames(dna.mat) <- paste0('doc', 1:nrow(dna.mat))
colnames(dna.mat) <- unique(reducedWordCount$word)
```

Split Test/Train 80/20.

``` r
set.seed(2019)
sample <- sample(1:nrow(dna.mat), 
                 (nrow(dna.mat) * .80) %>% ceiling(), replace = FALSE)

train.df      <- dna.mat[sample,  ]
train.product <- new.product[sample] %>% as.factor()
test.df       <- dna.mat[-sample, ]
test.product  <- new.product[-sample] %>% as.factor()
```

1. Exploration
--------------

``` r
#tree.grid <- expand.grid(mtry = nrow(train.df) %>% sqrt(), ntree = c(50, 100, 250)) %>% as.data.frame()

#tree.grid <- expand.grid(mtry = seq(4,16,4), ntree = c(700, 1000,2000) )
#Mixed alpha penalized reg.
glm.mod <- train(train.product ~ ., data = data.frame(train.product, train.df), method = "glmnet",
                 family = "binomial", trControl = trainControl(method = "cv", number = 10))

#Random forest w optimized mtry
#random.rf <- train(train.product ~ ., data = data.frame(train.product, train.df), method = "rf", family = "bernoulli",
#                   trControl = trainControl(method = "cv", number = 5, n = 50, mtry = 5))

random.rf <- train(train.product ~ ., data = data.frame(train.product, train.df), method = "rf", family = "bernoulli",
                  trControl = trainControl(method = "none"))

#Boosting trees
random.gmb <- train(train.product ~ ., data = data.frame(train.product, train.df), distribution = "bernoulli", method = "gbm", trControl = trainControl(method = "none"))
```

    ## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
    ##      1        1.3610            -nan     0.1000    0.0109
    ##      2        1.3409            -nan     0.1000    0.0083
    ##      3        1.3213            -nan     0.1000    0.0086
    ##      4        1.3024            -nan     0.1000    0.0093
    ##      5        1.2872            -nan     0.1000    0.0064
    ##      6        1.2742            -nan     0.1000    0.0070
    ##      7        1.2618            -nan     0.1000    0.0052
    ##      8        1.2511            -nan     0.1000    0.0043
    ##      9        1.2403            -nan     0.1000    0.0051
    ##     10        1.2312            -nan     0.1000    0.0024
    ##     20        1.1646            -nan     0.1000    0.0023
    ##     40        1.0819            -nan     0.1000    0.0014
    ##     50        1.0478            -nan     0.1000    0.0008

``` r
#Fixed/Default Sigmoid SVM
default.svm <- train(train.product ~ ., data = data.frame(train.product, train.df), distribution = "bernoulli", method = "svmLinear", trControl = trainControl(method = "none"))

#Modlist Accuracy
mod.list  <- list(glm.mod, random.rf, random.gmb, default.svm)
pred.list <- map(mod.list, ~predict(.x, newdata = data.frame(test.product, test.df)))
accuracy  <- map_dbl(pred.list, ~ (.x == test.product) %>% mean())
model     <- c("Penalized Regression", "Random Forest", "Boosting", "Default SVM")

#Visualize Accuracy in Table
data.frame(Accuracy = accuracy, Model = model) %>%
  knitr::kable(digits = 4)
```

|  Accuracy| Model                |
|---------:|:---------------------|
|    0.7487| Penalized Regression |
|    0.6935| Random Forest        |
|    0.6533| Boosting             |
|    0.6432| Default SVM          |

3. Visualize Preliminary Results
================================

``` r
pred.list <- map(mod.list[-4], ~predict(.x, newdata = data.frame(test.product, test.df), type = "prob")[,2])
glm.preds <- predict(glm.mod, newdata = data.frame(test.product, test.df))
glm.accuracy <- (glm.preds == test.product) %>% mean()

Roc.Log <- function(result,preds){
  probs <- seq(0,1,by=0.001)
  roc.log <- matrix(0,nrow=length(probs),ncol=2)
 # result <- ifelse(result == "M", TRUE, FALSE)
  i <- 1
  for(p in probs){
    pred <- preds > p
    ##False positive rate
    false.pos <- sum(!result & pred)/sum(!result)
    ##True postive rate
    true.pos <- sum(result & pred)/sum(result)
    roc.log[i,] <- c(false.pos,true.pos)
    i <- i+1
  }
  return(roc.log)
}

plotRoc <- function(roc.log,charstring){
  data.frame(FP =rev(roc.log[,1]),TP=rev(roc.log[,2])) %>% 
    ggplot()+
    geom_point(aes(FP,TP),color="red",size=.5) +
    geom_line(aes(FP,TP),color="blue") +
    geom_abline(slope = 1, linetype = 2, colour = "lightgrey") +
    labs(
      x = "False Positive",
      y = "True Positive",
      title = charstring
    )
}
auc <- function(roc){
  len <- nrow(roc)
  ##The "delta X" values
  delta <- roc[-1,1]-roc[-len,1]
  ##The "heights" the rectangle (drop the first or last).
  hgt <- roc[-1,2]
  ##The Riemann Sum
  sum(-delta*hgt)
}


#Visualize ROC w/AUC
result  <- test.product %>% as.logical()
roc.list <- map(.x = pred.list, ~Roc.Log(result, .x))
names <- c("Penalized GLM", "Random Forest", "Boosting")
plot.list <- map2(.x = roc.list, .y = names, ~plotRoc(.x, sprintf("%s | AUC: %s Percent", .y, auc(.x) %>% round(4)*100)))

ggplot <- (plot.list[[1]] + plot.list[[2]]) / plot.list[[3]]
ggplot
```

<img src="dna_sample_predictive_modeling_files/figure-markdown_github/unnamed-chunk-2-1.png" width="90%" />

``` r
ggsave("./src/nevilleq/dna_sample_predictive_modeling/all_test_roc.jpg", ggplot)
```

    ## Saving 6 x 5 in image

3. Tune Models
--------------

``` r
#tree.grid <- expand.grid(mtry = nrow(train.df) %>% sqrt(), ntree = c(50, 100, 250)) %>% as.data.frame()

#tree.grid <- expand.grid(mtry = seq(4,16,4), ntree = c(700, 1000,2000) )
#Mixed alpha penalized reg.
glm.mod <- train(train.product ~ ., data = data.frame(train.product, train.df), method = "glmnet",
                 family = "binomial", trControl = trainControl(method = "cv", number = 10))

#Random forest w optimized mtry
#random.rf <- train(train.product ~ ., data = data.frame(train.product, train.df), method = "rf", family = "bernoulli",
#                   trControl = trainControl(method = "cv", number = 5, n = 50, mtry = 5))

random.rf <- train(train.product ~ ., data = data.frame(train.product, train.df), method = "rf", family = "bernoulli",
                  trControl = trainControl(method = "cv", number = 5))

#Boosting trees
random.gmb <- train(train.product ~ ., data = data.frame(train.product, train.df), distribution = "bernoulli", method = "gbm", trControl = trainControl(method = "cv", number = 5))
```

    ## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
    ##      1        1.3605            -nan     0.1000    0.0132
    ##      2        1.3393            -nan     0.1000    0.0088
    ##      3        1.3215            -nan     0.1000    0.0083
    ##      4        1.3058            -nan     0.1000    0.0068
    ##      5        1.2896            -nan     0.1000    0.0066
    ##      6        1.2768            -nan     0.1000    0.0071
    ##      7        1.2682            -nan     0.1000    0.0035
    ##      8        1.2559            -nan     0.1000    0.0057
    ##      9        1.2465            -nan     0.1000    0.0042
    ##     10        1.2366            -nan     0.1000    0.0038
    ##     20        1.1606            -nan     0.1000    0.0009
    ##     40        1.0739            -nan     0.1000   -0.0003
    ##     60        1.0052            -nan     0.1000    0.0003
    ##     80        0.9504            -nan     0.1000   -0.0013
    ##    100        0.9052            -nan     0.1000   -0.0001
    ##    120        0.8703            -nan     0.1000   -0.0002
    ##    140        0.8386            -nan     0.1000   -0.0007
    ##    150        0.8226            -nan     0.1000   -0.0008
    ## 
    ## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
    ##      1        1.3486            -nan     0.1000    0.0151
    ##      2        1.3238            -nan     0.1000    0.0096
    ##      3        1.2960            -nan     0.1000    0.0137
    ##      4        1.2776            -nan     0.1000    0.0065
    ##      5        1.2562            -nan     0.1000    0.0102
    ##      6        1.2370            -nan     0.1000    0.0059
    ##      7        1.2198            -nan     0.1000    0.0055
    ##      8        1.2039            -nan     0.1000    0.0077
    ##      9        1.1859            -nan     0.1000    0.0064
    ##     10        1.1744            -nan     0.1000    0.0031
    ##     20        1.0741            -nan     0.1000    0.0020
    ##     40        0.9485            -nan     0.1000   -0.0013
    ##     60        0.8644            -nan     0.1000    0.0004
    ##     80        0.8032            -nan     0.1000   -0.0017
    ##    100        0.7488            -nan     0.1000   -0.0005
    ##    120        0.7062            -nan     0.1000   -0.0015
    ##    140        0.6685            -nan     0.1000   -0.0013
    ##    150        0.6511            -nan     0.1000   -0.0005
    ## 
    ## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
    ##      1        1.3426            -nan     0.1000    0.0184
    ##      2        1.3045            -nan     0.1000    0.0160
    ##      3        1.2777            -nan     0.1000    0.0094
    ##      4        1.2465            -nan     0.1000    0.0092
    ##      5        1.2192            -nan     0.1000    0.0084
    ##      6        1.1966            -nan     0.1000    0.0077
    ##      7        1.1751            -nan     0.1000    0.0093
    ##      8        1.1584            -nan     0.1000    0.0051
    ##      9        1.1447            -nan     0.1000    0.0043
    ##     10        1.1288            -nan     0.1000    0.0051
    ##     20        1.0032            -nan     0.1000    0.0005
    ##     40        0.8643            -nan     0.1000   -0.0001
    ##     60        0.7641            -nan     0.1000   -0.0012
    ##     80        0.6938            -nan     0.1000   -0.0012
    ##    100        0.6341            -nan     0.1000   -0.0017
    ##    120        0.5808            -nan     0.1000   -0.0007
    ##    140        0.5382            -nan     0.1000   -0.0008
    ##    150        0.5214            -nan     0.1000   -0.0010

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution =
    ## "bernoulli", : variable 1092: cam has no variation.

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution =
    ## "bernoulli", : variable 1102: strap has no variation.

    ## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
    ##      1        1.3635            -nan     0.1000    0.0098
    ##      2        1.3404            -nan     0.1000    0.0119
    ##      3        1.3239            -nan     0.1000    0.0073
    ##      4        1.3026            -nan     0.1000    0.0086
    ##      5        1.2855            -nan     0.1000    0.0069
    ##      6        1.2759            -nan     0.1000    0.0039
    ##      7        1.2662            -nan     0.1000    0.0032
    ##      8        1.2548            -nan     0.1000    0.0058
    ##      9        1.2430            -nan     0.1000    0.0049
    ##     10        1.2322            -nan     0.1000    0.0057
    ##     20        1.1585            -nan     0.1000    0.0022
    ##     40        1.0715            -nan     0.1000   -0.0006
    ##     60        1.0125            -nan     0.1000    0.0003
    ##     80        0.9660            -nan     0.1000   -0.0017
    ##    100        0.9266            -nan     0.1000   -0.0000
    ##    120        0.8908            -nan     0.1000   -0.0006
    ##    140        0.8568            -nan     0.1000   -0.0012
    ##    150        0.8417            -nan     0.1000    0.0002

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution =
    ## "bernoulli", : variable 1092: cam has no variation.

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution =
    ## "bernoulli", : variable 1102: strap has no variation.

    ## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
    ##      1        1.3512            -nan     0.1000    0.0150
    ##      2        1.3184            -nan     0.1000    0.0157
    ##      3        1.2930            -nan     0.1000    0.0123
    ##      4        1.2703            -nan     0.1000    0.0091
    ##      5        1.2458            -nan     0.1000    0.0098
    ##      6        1.2265            -nan     0.1000    0.0088
    ##      7        1.2113            -nan     0.1000    0.0055
    ##      8        1.1986            -nan     0.1000    0.0034
    ##      9        1.1855            -nan     0.1000    0.0041
    ##     10        1.1702            -nan     0.1000    0.0061
    ##     20        1.0739            -nan     0.1000    0.0028
    ##     40        0.9550            -nan     0.1000   -0.0023
    ##     60        0.8717            -nan     0.1000    0.0002
    ##     80        0.8075            -nan     0.1000   -0.0001
    ##    100        0.7532            -nan     0.1000   -0.0002
    ##    120        0.7065            -nan     0.1000   -0.0009
    ##    140        0.6633            -nan     0.1000   -0.0009
    ##    150        0.6474            -nan     0.1000   -0.0012

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution =
    ## "bernoulli", : variable 1092: cam has no variation.

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution =
    ## "bernoulli", : variable 1102: strap has no variation.

    ## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
    ##      1        1.3431            -nan     0.1000    0.0192
    ##      2        1.3024            -nan     0.1000    0.0171
    ##      3        1.2707            -nan     0.1000    0.0131
    ##      4        1.2458            -nan     0.1000    0.0094
    ##      5        1.2188            -nan     0.1000    0.0096
    ##      6        1.1960            -nan     0.1000    0.0097
    ##      7        1.1719            -nan     0.1000    0.0076
    ##      8        1.1549            -nan     0.1000    0.0057
    ##      9        1.1379            -nan     0.1000    0.0037
    ##     10        1.1221            -nan     0.1000    0.0054
    ##     20        1.0102            -nan     0.1000    0.0018
    ##     40        0.8616            -nan     0.1000    0.0009
    ##     60        0.7617            -nan     0.1000   -0.0005
    ##     80        0.6870            -nan     0.1000   -0.0007
    ##    100        0.6235            -nan     0.1000   -0.0011
    ##    120        0.5722            -nan     0.1000   -0.0005
    ##    140        0.5326            -nan     0.1000   -0.0020
    ##    150        0.5138            -nan     0.1000   -0.0006

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution =
    ## "bernoulli", : variable 1101: waveguide has no variation.

    ## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
    ##      1        1.3615            -nan     0.1000    0.0107
    ##      2        1.3384            -nan     0.1000    0.0065
    ##      3        1.3196            -nan     0.1000    0.0091
    ##      4        1.3023            -nan     0.1000    0.0066
    ##      5        1.2884            -nan     0.1000    0.0067
    ##      6        1.2791            -nan     0.1000    0.0014
    ##      7        1.2631            -nan     0.1000    0.0079
    ##      8        1.2520            -nan     0.1000    0.0023
    ##      9        1.2364            -nan     0.1000    0.0055
    ##     10        1.2286            -nan     0.1000    0.0032
    ##     20        1.1599            -nan     0.1000    0.0015
    ##     40        1.0625            -nan     0.1000    0.0001
    ##     60        1.0032            -nan     0.1000    0.0001
    ##     80        0.9507            -nan     0.1000    0.0007
    ##    100        0.9031            -nan     0.1000    0.0000
    ##    120        0.8642            -nan     0.1000    0.0007
    ##    140        0.8308            -nan     0.1000    0.0000
    ##    150        0.8135            -nan     0.1000   -0.0005

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution =
    ## "bernoulli", : variable 1101: waveguide has no variation.

    ## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
    ##      1        1.3440            -nan     0.1000    0.0186
    ##      2        1.3153            -nan     0.1000    0.0137
    ##      3        1.2864            -nan     0.1000    0.0105
    ##      4        1.2633            -nan     0.1000    0.0099
    ##      5        1.2483            -nan     0.1000    0.0055
    ##      6        1.2315            -nan     0.1000    0.0067
    ##      7        1.2201            -nan     0.1000    0.0016
    ##      8        1.1990            -nan     0.1000    0.0084
    ##      9        1.1796            -nan     0.1000    0.0070
    ##     10        1.1659            -nan     0.1000    0.0029
    ##     20        1.0726            -nan     0.1000    0.0021
    ##     40        0.9431            -nan     0.1000    0.0011
    ##     60        0.8555            -nan     0.1000   -0.0003
    ##     80        0.7834            -nan     0.1000   -0.0005
    ##    100        0.7327            -nan     0.1000   -0.0005
    ##    120        0.6855            -nan     0.1000   -0.0002
    ##    140        0.6502            -nan     0.1000   -0.0007
    ##    150        0.6317            -nan     0.1000   -0.0005

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution =
    ## "bernoulli", : variable 1101: waveguide has no variation.

    ## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
    ##      1        1.3382            -nan     0.1000    0.0210
    ##      2        1.3030            -nan     0.1000    0.0138
    ##      3        1.2711            -nan     0.1000    0.0126
    ##      4        1.2433            -nan     0.1000    0.0118
    ##      5        1.2145            -nan     0.1000    0.0104
    ##      6        1.1884            -nan     0.1000    0.0096
    ##      7        1.1686            -nan     0.1000    0.0082
    ##      8        1.1492            -nan     0.1000    0.0064
    ##      9        1.1335            -nan     0.1000    0.0046
    ##     10        1.1208            -nan     0.1000    0.0018
    ##     20        1.0008            -nan     0.1000    0.0010
    ##     40        0.8601            -nan     0.1000    0.0006
    ##     60        0.7595            -nan     0.1000   -0.0003
    ##     80        0.6832            -nan     0.1000   -0.0009
    ##    100        0.6200            -nan     0.1000   -0.0007
    ##    120        0.5683            -nan     0.1000   -0.0015
    ##    140        0.5288            -nan     0.1000   -0.0006
    ##    150        0.5103            -nan     0.1000   -0.0012
    ## 
    ## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
    ##      1        1.3627            -nan     0.1000    0.0100
    ##      2        1.3433            -nan     0.1000    0.0079
    ##      3        1.3269            -nan     0.1000    0.0064
    ##      4        1.3093            -nan     0.1000    0.0054
    ##      5        1.2963            -nan     0.1000    0.0022
    ##      6        1.2841            -nan     0.1000    0.0057
    ##      7        1.2716            -nan     0.1000    0.0056
    ##      8        1.2614            -nan     0.1000    0.0039
    ##      9        1.2557            -nan     0.1000    0.0013
    ##     10        1.2452            -nan     0.1000    0.0041
    ##     20        1.1742            -nan     0.1000    0.0024
    ##     40        1.0815            -nan     0.1000   -0.0003
    ##     60        1.0150            -nan     0.1000    0.0003
    ##     80        0.9643            -nan     0.1000    0.0006
    ##    100        0.9224            -nan     0.1000   -0.0009
    ##    120        0.8821            -nan     0.1000   -0.0006
    ##    140        0.8483            -nan     0.1000    0.0003
    ##    150        0.8332            -nan     0.1000   -0.0001
    ## 
    ## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
    ##      1        1.3493            -nan     0.1000    0.0158
    ##      2        1.3189            -nan     0.1000    0.0140
    ##      3        1.2956            -nan     0.1000    0.0090
    ##      4        1.2722            -nan     0.1000    0.0103
    ##      5        1.2511            -nan     0.1000    0.0089
    ##      6        1.2366            -nan     0.1000    0.0061
    ##      7        1.2222            -nan     0.1000    0.0065
    ##      8        1.2046            -nan     0.1000    0.0057
    ##      9        1.1911            -nan     0.1000    0.0028
    ##     10        1.1782            -nan     0.1000    0.0051
    ##     20        1.0792            -nan     0.1000    0.0015
    ##     40        0.9603            -nan     0.1000    0.0005
    ##     60        0.8733            -nan     0.1000    0.0002
    ##     80        0.8038            -nan     0.1000   -0.0009
    ##    100        0.7512            -nan     0.1000    0.0003
    ##    120        0.7031            -nan     0.1000   -0.0010
    ##    140        0.6628            -nan     0.1000   -0.0009
    ##    150        0.6455            -nan     0.1000    0.0001
    ## 
    ## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
    ##      1        1.3464            -nan     0.1000    0.0186
    ##      2        1.3103            -nan     0.1000    0.0158
    ##      3        1.2760            -nan     0.1000    0.0116
    ##      4        1.2552            -nan     0.1000    0.0078
    ##      5        1.2283            -nan     0.1000    0.0103
    ##      6        1.2049            -nan     0.1000    0.0090
    ##      7        1.1880            -nan     0.1000    0.0039
    ##      8        1.1740            -nan     0.1000    0.0037
    ##      9        1.1562            -nan     0.1000    0.0056
    ##     10        1.1396            -nan     0.1000    0.0067
    ##     20        1.0139            -nan     0.1000    0.0030
    ##     40        0.8694            -nan     0.1000    0.0004
    ##     60        0.7744            -nan     0.1000    0.0004
    ##     80        0.7003            -nan     0.1000   -0.0027
    ##    100        0.6476            -nan     0.1000   -0.0006
    ##    120        0.5962            -nan     0.1000   -0.0014
    ##    140        0.5565            -nan     0.1000   -0.0001
    ##    150        0.5387            -nan     0.1000   -0.0009
    ## 
    ## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
    ##      1        1.3582            -nan     0.1000    0.0121
    ##      2        1.3354            -nan     0.1000    0.0095
    ##      3        1.3149            -nan     0.1000    0.0093
    ##      4        1.2986            -nan     0.1000    0.0068
    ##      5        1.2840            -nan     0.1000    0.0082
    ##      6        1.2692            -nan     0.1000    0.0067
    ##      7        1.2569            -nan     0.1000    0.0047
    ##      8        1.2450            -nan     0.1000    0.0056
    ##      9        1.2301            -nan     0.1000    0.0039
    ##     10        1.2228            -nan     0.1000    0.0018
    ##     20        1.1486            -nan     0.1000    0.0016
    ##     40        1.0563            -nan     0.1000   -0.0001
    ##     60        0.9915            -nan     0.1000    0.0002
    ##     80        0.9388            -nan     0.1000    0.0001
    ##    100        0.8970            -nan     0.1000    0.0000
    ##    120        0.8597            -nan     0.1000   -0.0003
    ##    140        0.8262            -nan     0.1000   -0.0001
    ##    150        0.8114            -nan     0.1000   -0.0007
    ## 
    ## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
    ##      1        1.3464            -nan     0.1000    0.0185
    ##      2        1.3156            -nan     0.1000    0.0136
    ##      3        1.2827            -nan     0.1000    0.0134
    ##      4        1.2556            -nan     0.1000    0.0110
    ##      5        1.2314            -nan     0.1000    0.0103
    ##      6        1.2113            -nan     0.1000    0.0080
    ##      7        1.1929            -nan     0.1000    0.0072
    ##      8        1.1780            -nan     0.1000    0.0062
    ##      9        1.1615            -nan     0.1000    0.0057
    ##     10        1.1506            -nan     0.1000    0.0029
    ##     20        1.0574            -nan     0.1000    0.0005
    ##     40        0.9371            -nan     0.1000   -0.0014
    ##     60        0.8534            -nan     0.1000   -0.0015
    ##     80        0.7864            -nan     0.1000   -0.0014
    ##    100        0.7315            -nan     0.1000   -0.0006
    ##    120        0.6787            -nan     0.1000   -0.0006
    ##    140        0.6348            -nan     0.1000   -0.0008
    ##    150        0.6160            -nan     0.1000   -0.0021
    ## 
    ## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
    ##      1        1.3415            -nan     0.1000    0.0193
    ##      2        1.3028            -nan     0.1000    0.0165
    ##      3        1.2651            -nan     0.1000    0.0152
    ##      4        1.2327            -nan     0.1000    0.0135
    ##      5        1.2143            -nan     0.1000    0.0064
    ##      6        1.1863            -nan     0.1000    0.0138
    ##      7        1.1612            -nan     0.1000    0.0087
    ##      8        1.1385            -nan     0.1000    0.0093
    ##      9        1.1218            -nan     0.1000    0.0065
    ##     10        1.1043            -nan     0.1000    0.0054
    ##     20        0.9988            -nan     0.1000    0.0021
    ##     40        0.8564            -nan     0.1000    0.0001
    ##     60        0.7532            -nan     0.1000   -0.0002
    ##     80        0.6762            -nan     0.1000   -0.0012
    ##    100        0.6182            -nan     0.1000    0.0000
    ##    120        0.5667            -nan     0.1000   -0.0007
    ##    140        0.5205            -nan     0.1000   -0.0004
    ##    150        0.5020            -nan     0.1000   -0.0000
    ## 
    ## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
    ##      1        1.3516            -nan     0.1000    0.0167
    ##      2        1.3208            -nan     0.1000    0.0129
    ##      3        1.2968            -nan     0.1000    0.0111
    ##      4        1.2723            -nan     0.1000    0.0112
    ##      5        1.2513            -nan     0.1000    0.0087
    ##      6        1.2299            -nan     0.1000    0.0097
    ##      7        1.2121            -nan     0.1000    0.0062
    ##      8        1.1987            -nan     0.1000    0.0056
    ##      9        1.1833            -nan     0.1000    0.0064
    ##     10        1.1665            -nan     0.1000    0.0057
    ##     20        1.0794            -nan     0.1000    0.0007
    ##     40        0.9639            -nan     0.1000    0.0006
    ##     60        0.8908            -nan     0.1000    0.0006
    ##     80        0.8261            -nan     0.1000   -0.0001
    ##    100        0.7731            -nan     0.1000   -0.0012
    ##    120        0.7282            -nan     0.1000   -0.0006
    ##    140        0.6898            -nan     0.1000   -0.0010
    ##    150        0.6730            -nan     0.1000   -0.0007

``` r
#Fixed/Default Sigmoid SVM
default.svm <- train(train.product ~ ., data = data.frame(train.product, train.df), distribution = "bernoulli", method = "svmLinear", trControl = trainControl(method = "cv", number = 5))
```

    ## Warning in .local(x, ...): Variable(s) `' constant. Cannot scale data.

    ## Warning in .local(x, ...): Variable(s) `' constant. Cannot scale data.

``` r
#Modlist Accuracy
mod.list  <- list(glm.mod, random.rf, random.gmb, default.svm)
pred.list <- map(mod.list, ~predict(.x, newdata = data.frame(test.product, test.df)))
accuracy  <- map_dbl(pred.list, ~ (.x == test.product) %>% mean())
model     <- c("Penalized Regression", "Random Forest", "Boosting", "Default SVM")

#Visualize Accuracy in Table
data.frame(Accuracy = accuracy, Model = model) %>%
  knitr::kable(digits = 4)
```

|  Accuracy| Model                |
|---------:|:---------------------|
|    0.7487| Penalized Regression |
|    0.7035| Random Forest        |
|    0.7136| Boosting             |
|    0.6432| Default SVM          |

``` r
pred.list <- map(mod.list[-4], ~predict(.x, newdata = data.frame(test.product, test.df), type = "prob")[,2])


#Visualize ROC w/AUC
result  <- test.product %>% as.logical()
roc.list <- map(.x = pred.list, ~Roc.Log(result, .x))
names <- c("Penalized GLM", "Random Forest", "Boosting")
plot.list <- map2(.x = roc.list, .y = names, ~plotRoc(.x, sprintf("%s | AUC: %s Percent", .y, auc(.x) %>% round(4)*100)))

ggplot <- (plot.list[[1]] + plot.list[[2]]) / plot.list[[3]]
ggplot
```

<img src="dna_sample_predictive_modeling_files/figure-markdown_github/unnamed-chunk-4-1.png" width="90%" />

``` r
ggsave("./src/nevilleq/dna_sample_predictive_modeling/all_test_test_roc.jpg", ggplot)
```

    ## Saving 6 x 5 in image

3. Visualize Preliminary Results
================================

4. Visualize All Models
=======================

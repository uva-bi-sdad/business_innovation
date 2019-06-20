#################################
##########DNA SAMPLE#############
#################################
#######By: Quinton Neville#######
#################################

#Libraries
library(tidyverse)
library(data.table)
library(matrixcalc)
library(Matrix)
library(MASS)
library(purrr)

####Testing####

#Generate some synthetic data
N <- 100
K <- 10
p <- runif(1)

cont.dt <- mvrnorm(n = N, mu = sample(seq(-2, 2, by = 0.01), K, replace = TRUE),
                   Sigma = diag(sample(seq(0.1, 3, by = 0.01), K, replace = TRUE))) %>%
           as.tibble()


cat.dt  <- tibble(
  binary    = sample(c(0, 1), N, prob = c(p, 1-p), replace = TRUE) %>% as.character(),
  cat_small = sample(1:5, N, replace = TRUE) %>% as.character(),
  cat_large = sample(LETTERS, N, replace = TRUE) %>% as.character()
)

sim.dt <- bind_cols(cont.dt, cat.dt) %>% as.data.table()
rm(cont.dt, cat.dt)

#Random Sampling by Kolmogorov-Smirnov & Chisq

sample  <- sample(1:N, K, replace = FALSE)
samp.dt <- sim.dt[sample, ]

#Continuous Single Test
ks.test(samp.dt[["V1"]], sim.dt[["V1"]], alternative = "two.sided", exact = FALSE)$p.value

#Continuous p-values by Kolmogorov-Smirnov
cont.p.vals <- map2_dbl(.x = samp.dt[ , which(sapply(samp.dt, is.numeric)), with = FALSE],
                        .y = sim.dt[ , which(sapply(sim.dt, is.numeric)), with = FALSE],
                        ~ks.test(.x, .y, alternative = "two.sided", exact = FALSE)$p.value)

#Categorical Single Test
chisq.test(
  cbind(table(samp.dt$binary), table(sim.dt$binary))
)$p.value

chisq.test(
  cbind(table(samp.dt$cat_small), table(sim.dt$cat_small))
)$p.value

chisq.test(
cbind(table(samp.dt$cat_large), table(sim.dt$cat_large))
)$p.value

#Categorical p-values by Chisq test of independence
cat.p.vals <- map2_dbl(.x = samp.dt[ , which(sapply(samp.dt, is.character)), with = FALSE],
                       .y = sim.dt[ , which(sapply(sim.dt, is.character)), with = FALSE],
                       ~chisq.test(
                         cbind(table(.x), table(.y))
                       )$p.value)

#Result
result <- c(cont.p.vals, cat.p.vals)
#Any significant results (i.e. rejections that they are not from the same dist/independent)
any(result < 0.05)

###Wrap it up into recursive function
random_sample_recursive <- function(sim.dt, samp.size, start.seed) {
  seed <- start.seed
  set.seed(seed)
  samp.dt <- sim.dt[sample(1:nrow(sim.dt), samp.size, replace = FALSE), ]

  #Continuous p-values by Kolmogorov-Smirnov
  p.vals <- c(map2_dbl(.x = samp.dt[ , which(sapply(samp.dt, is.numeric)), with = FALSE],
                          .y = sim.dt[ , which(sapply(sim.dt, is.numeric)), with = FALSE],
                          ~ks.test(.x, .y, alternative = "two.sided", exact = FALSE)$p.value),
              map2_dbl(.x = samp.dt[ , which(sapply(samp.dt, is.character)), with = FALSE],
                       .y = sim.dt[ , which(sapply(sim.dt, is.character)), with = FALSE],
                       ~chisq.test(
                         cbind(table(.x), table(.y))
                       )$p.value)
              )

  #Any significant results (i.e. rejections that they are not from the same dist/independent)
  ifelse(any(p.vals < 0.05),
         random_sample_recursive(sim.df, samp.size, (seed + 1)),
         return(list(dt = samp.dt, seed = seed)))
}

###Wrap it up into parallel function
random_sample_parallel <- function(sim.dt, samp.size) {
  seed <- rdunif(1, 1, 100000) %>% setNames("seed")
  set.seed(seed)
  samp.dt <- sim.dt[sample(1:nrow(sim.dt), samp.size, replace = FALSE), ]

  #Continuous p-values by Kolmogorov-Smirnov
  p.vals <- c(map2_dbl(.x = samp.dt[ , which(sapply(samp.dt, is.numeric)), with = FALSE],
                       .y = sim.dt[ , which(sapply(sim.dt, is.numeric)), with = FALSE],
                       ~ks.test(.x, .y, alternative = "two.sided", exact = FALSE)$p.value),
              map2_dbl(.x = samp.dt[ , which(sapply(samp.dt, is.character)), with = FALSE],
                       .y = sim.dt[ , which(sapply(sim.dt, is.character)), with = FALSE],
                       ~chisq.test(
                         cbind(table(.x), table(.y))
                       )$p.value)
  )

  #Any significant results (i.e. rejections that they are not from the same dist/independent)

  return(c(p.vals, seed))
}

###Test Parallelizing
library(doParallel)
library(foreach)
library(parallel)

#Set up Cores, Task Function
(NCores  <- detectCores())
UseCores <- NCores/2
taskFun  <- random_sample_parallel
registerDoParallel(UseCores)

#1 X 10 = 10 iterations
result.mat <- foreach(i = 1:10, .combine = rbind, .multicombine = TRUE,
                         .packages = c("data.table")) %dopar% {
                           outSub <- taskFun(sim.df, samp.size = 10)
                         }

#Find which iterations had a sample with all fail to reject null
log.mat <- result.mat[ ,-14] > 0.10
test <- apply(log.mat, 1, function(x) {x %>% all() %>% isTRUE()})
true.samp.seeds <- result.mat[which(test == TRUE), 14]

#Randomly select the seed we use
seed <- sample(true.samp.seeds, 1) %>% unname()




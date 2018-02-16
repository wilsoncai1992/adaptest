composition_out <- get_composition(adaptest_fit, 'big')
composition_out <- get_composition(adaptest_fit)
composition_out
bootstrap_once <- function(data, parameter_wrapper = adaptest::rank_DE) {
  Y <- data$Y
  A <- data$A

  # non-parametric bootstrap
  SAMPLE_PER_BOOTSTRAP <- length(A)
  indices <- sample(1:length(A), size = SAMPLE_PER_BOOTSTRAP, replace = TRUE) # user specify sample size
  Y = Y[indices,]
  A = A[indices]

  # calc rank
  rank_out <- parameter_wrapper(Y = Y, A = A, W = rep(1, SAMPLE_PER_BOOTSTRAP), absolute = FALSE, negative = FALSE)
  return(rank_out)
}
# =============================================================================
N_SIMULATION = 1e3
# N_SIMULATION = 8
library(foreach)
# library(Rmpi)
# library(doMPI)
# cl = startMPIcluster()
# registerDoMPI(cl)
# clusterSize(cl) # just to check

library(doSNOW)
library(tcltk)
nw <- parallel:::detectCores()  # number of workers
cl <- makeSOCKcluster(nw)
registerDoSNOW(cl)

all_CI <- foreach(it2 = 1:(N_SIMULATION),
                  .combine = rbind,
                  .packages = c('R6', 'SuperLearner', 'adaptest'),
                  .inorder = FALSE,
                  .errorhandling = 'pass',
                  .verbose = T) %dopar% {
                    # .verbose = T) %do% {
                    bootstrap_once(data = simulated, parameter_wrapper = rank_DE)
                    # bootstrap_once(data = simulated)
                  }
head(all_CI)

# boxplot.matrix(all_CI[,1:30])
# plot(colMeans(all_CI))

# create df
alpha = 0.5
lower <- apply(all_CI, 2, function(x) quantile(x, alpha/2))
upper <- apply(all_CI, 2, function(x) quantile(x, 1-alpha/2))
df_CI <- data.frame(id = 1:ncol(simulated$Y), lower = lower, upper = upper)

library(dplyr)
df_CI %>% filter(lower <= 1) %>% filter(upper >= 1)
df_CI %>% filter(lower <= 10) %>% filter(upper >= 10)
df_CI %>% filter(lower <= 9) %>% filter(upper >= 9)

df_CI %>% filter(lower <= 10) %>% filter(upper >= 1)
df_CI %>% filter(lower <= 5) %>% filter(upper >= 1)

# combine with adaptest
adaptest_param <- as.integer(rownames(composition_out[[1]]))
yi <- sapply(adaptest_param, function(x) df_CI %>% filter(lower <= x) %>% filter(upper >= x) %>% select(id) %>% .$id)
unique(do.call(c, yi))

composition_out[[2]]
colSums(composition_out[[1]])[colSums(composition_out[[1]]) >= 0.5]

colSums(composition_out[[1]][1:15,])[colSums(composition_out[[1]][1:15,]) >= .3]

n.sim <- 1e2 # sample size
# n.sim <- 1 # sample size
p.all <- 1e3 # No. of dimensions of Y (signal + non-signal)
# p.all <- 1e6 # No. of dimensions of Y (signal + non-signal)
p.true <- 10 # No. of dimensions for signal
signal.true <- 1 # size of true signal
# signal.true <- .5 # size of true signal

# n.top.want <- p.true
n.top.want <- 15
n.fold <- 10
# ==============================================================================
# simulate data
# ==============================================================================
library(MASS)
library(Matrix)
# ------------------------------------------------------------------------------
# epsilon
epsilon <- matrix(rnorm(n = n.sim * p.all), nrow = n.sim, ncol = p.all)

# ------------------------------------------------------------------------------
# A
A.candidate <- list(rep(1, p.all), rep(0, p.all))
A.sample <- sample(A.candidate, size = n.sim, replace = TRUE)

A.sample <- do.call(rbind, A.sample)
A.sample.vec <- A.sample[,1]

# ------------------------------------------------------------------------------
# B1
b1.row <- c(rep(signal.true, p.true), rep(0, p.all - p.true))

rep.row<-function(x,n){
	matrix(rep(x,each=n),nrow=n)
}
b1 <- rep.row(b1.row, n = n.sim)
# ------------------------------------------------------------------------------
# B0
b0.row <- rnorm(n = p.all)
b0 <- rep.row(b0.row, n = n.sim)
# ------------------------------------------------------------------------------
# Y
temp1 <- b1 * A.sample
# ------------------------------------
rm(list = c('b1', 'A.sample'))
gc()
# ------------------------------------
Y <- b0 + temp1 + epsilon
# ------------------------------------
rm(list = c('b0', 'epsilon', 'temp1'))
gc()
# ------------------------------------

# ==============================================================================
# fit model
# ==============================================================================

# setwd('/Users/wilsoncai/Desktop/Data adaptive CVTMLE/code/dev/')
source('./R/data_adapt.R')
source('./R/data_adapt_multi_test.R')
source('./R/print.data_adapt.R')
source('./R/plot.data_adapt.R')
source('./R/data_adapt_rank.R')
out_result <- data_adapt_multi_test(Y = Y, A = A.sample.vec, n.top = p.all,
																		n.fold = 10, parallel = TRUE) # BH on all Y

print.adaptive(out_result)
plot.adaptive(out_result)

save(out_result, file = 'out_result_1e3_sig_1.Rdata')

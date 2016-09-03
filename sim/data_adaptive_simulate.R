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
# ==============================================================================================================
# simulate data
# ==============================================================================================================
library(MASS)
library(Matrix)
# ------------------------------------------------------------------------------------------------------------
# epsilon

## slow using mvnorm
# epsilon <- mvrnorm(n <- n.sim,
# 									 mu = rep(0, p.all),
# 									 Sigma = Diagonal(p.all))

# fast using 1-d rnorm
epsilon <- matrix(rnorm(n = n.sim * p.all), nrow = n.sim, byrow = TRUE)
# ------------------------------------------------------------------------------------------------------------
# A
A.candidate <- list(rep(1, p.all), rep(0, p.all))
A.sample <- sample(A.candidate, size = n.sim, replace = TRUE)

A.sample <- do.call(rbind, A.sample)
A.sample.vec <- A.sample[,1]
# library(data.table)
# rbindlist(A.sample)
# dim(rbind.fill.matrix())
# ------------------------------------------------------------------------------------------------------------
# B1
# b1.row <- c(rep(0.05, p.true), rep(0, p.all - p.true))
# b1.row <- c(rep(0.1, p.true), rep(0, p.all - p.true))
b1.row <- c(rep(signal.true, p.true), rep(0, p.all - p.true))
# b1.row <- c(rep(10, p.true), rep(0, p.all - p.true))
rep.row<-function(x,n){
	matrix(rep(x,each=n),nrow=n)
}
b1 <- rep.row(b1.row, n = n.sim)
# ------------------------------------------------------------------------------------------------------------
# B0
b0.row <- rnorm(n = p.all)
b0 <- rep.row(b0.row, n = n.sim)
# ------------------------------------------------------------------------------------------------------------
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

# ==============================================================================================================
# fit model
# ==============================================================================================================

## setwd('../5-25/')
# source('./data_adapt.ATE.R')
## data_adapt.ATE(Y = Y, A = A.sample.vec, n.top.want = 500, n.fold = 10)
# data_adapt.ATE(Y = Y, A = A.sample.vec, n.top.want = 15, n.fold = 10)
# data_adapt.ATE(Y = Y, A = A.sample.vec, n.top.want = p.all, n.fold = 10) # BH on all Y


# setwd('/Users/wilsoncai/Desktop/Data adaptive CVTMLE/code/dev/')
source('./data_adapt.multi_test.R')
source('./print.adaptive.R')
source('./plot.adaptive.R')
# out_result <- data_adapt.multi_test(Y = Y, A = A.sample.vec, n.top.want = 15, n.fold = 10)
# out_result <- data_adapt.multi_test(Y = Y, A = A.sample.vec, n.top.want = 50, n.fold = 10)
# out_result <- data_adapt.multi_test(Y = Y, A = A.sample.vec, n.top.want = 15, n.fold = 10, parallel = TRUE)
# out_result <- data_adapt.multi_test(Y = Y, A = A.sample.vec, n.top.want = 15, n.fold = 20, parallel = TRUE)
out_result <- data_adapt.multi_test(Y = Y, A = A.sample.vec, n.top.want = p.all, n.fold = 10, parallel = TRUE) # BH on all Y

print.adaptive(out_result)
plot.adaptive(out_result)

save(out_result, file = 'out_result_1e3_sig_1.Rdata')
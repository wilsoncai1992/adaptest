set.seed(4321)
n.sim <- 1e2 # sample size
# n.sim <- 1e3 # sample size
# n.sim <- 1 # sample size
# p.all <- 1e3 # No. of dimensions of Y (signal + non-signal)
p.all <- 1e4 # No. of dimensions of Y (signal + non-signal)
# p.all <- 1e5 # No. of dimensions of Y (signal + non-signal)
# p.all <- 1e6 # No. of dimensions of Y (signal + non-signal)
p.true <- 10 # No. of dimensions for signal
# signal.true <- 1 # size of true signal
signal.true <- .6 # size of true signal
# signal.true <- .8 # size of true signal
signal.true.W <- 0.1

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
# fit DA.test
# ==============================================================================
library(data.adapt.multi.test)
out_result <- data_adapt_multi_test(Y = Y, A = A.sample.vec, n.top = p.true + 5,
                                    # n.fold = 3, parallel = FALSE) # BH on all Y
                                    n.fold = 4, parallel = FALSE) # BH on all Y
print(out_result)
plot(out_result)
# print.data_adapt(out_result)
# plot.data_adapt(out_result)

save(out_result, file = 'out_result_1e3_sig_1.Rdata')
# ==============================================================================
# fit naive BH
# ==============================================================================
lm_out <- lm(Y ~ A.sample.vec)
B1_hat_lm <- lm_out$coefficients[2,]
names(B1_hat_lm) <- c(1:ncol(Y))

lm_summary <- summary(lm_out)
pval_lm <- sapply(lm_summary, function(x) x$coefficients[2,4])
qval_lm <- p.adjust(pval_lm, method = 'BH')
which(qval_lm < .05)
head(sort(qval_lm), 20)
plot(head(sort(qval_lm), 100))
plot(sort(qval_lm))

head(B1_hat_lm[c(1:ncol(Y))[order(qval_lm)]], 20)

head(sort(B1_hat_lm, decreasing = T), 20)

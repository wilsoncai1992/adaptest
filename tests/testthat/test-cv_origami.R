################################################################################
# setup
################################################################################
set.seed(628957)
library(MASS)
library(Matrix)
library(future)
library(adaptest)
context("adaptest works the same under sequential and multicore evaluation")

################################################################################
# simulation
################################################################################
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

# epsilon
epsilon <- matrix(rnorm(n = n.sim * p.all), nrow = n.sim, ncol = p.all)

# A
A.candidate <- list(rep(1, p.all), rep(0, p.all))
A.sample <- sample(A.candidate, size = n.sim, replace = TRUE)
A.sample <- do.call(rbind, A.sample)
A.sample.vec <- A.sample[,1]

# B1
b1.row <- c(rep(signal.true, p.true), rep(0, p.all - p.true))
rep.row<-function(x,n){
  matrix(rep(x,each=n),nrow=n)
}
b1 <- rep.row(b1.row, n = n.sim)

# B0
b0.row <- rnorm(n = p.all)
b0 <- rep.row(b0.row, n = n.sim)

# Y
temp1 <- b1 * A.sample
Y <- b0 + temp1 + epsilon

# clean up
rm(list = c('b1', 'A.sample'))
rm(list = c('b0', 'epsilon', 'temp1'))


################################################################################
# adaptest should be fast with futures...
################################################################################
plan(sequential)
set.seed(48915672)
time_seq <- system.time(
    result_seq <- adaptest(Y = Y, A = A.sample.vec, n_top = p.true + 5,
                           n_fold = 4)
)

plan(multiprocess)
set.seed(48915672)
time_mc <- system.time(
    set.seed(4321)
    result_mc <- adaptest(Y = Y, A = A.sample.vec, n_top = p.true + 5,
                          n_fold = 4)
)

test_that("Multiprocess and sequential evaluation return identical objects", {
  expect_equal(result_seq, result_mc)
})

if(availableCores() > 1) {
  test_that("Multiprocess evaluation is faster than sequential evaluation", {
    skip_on_os("windows") # Windows doesn't support multicore
    expect_lt(time_mc["elapsed"], time_seq["elapsed"])
  })
}


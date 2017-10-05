
n.sim <- 1e2 # sample size
p.all <- 1e4 # No. of dimensions of Y (signal + non-signal)
p.true <- 10 # No. of dimensions for signal
signal.true <- .6 # size of true signal
n.top.want <- 15
n.fold <- 10


simulate_once <- function(n.sim, p.all, p.true, signal.true, n.top.want, n.fold) {
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
    # rm(list = c('b0', 'epsilon', 'temp1'))
    rm(list = c('b0', 'temp1'))
    gc()
    return(list(Y, A.sample.vec))
}
# ========================================================================================

data_here <- simulate_once(n.sim, p.all, p.true, signal.true, n.top.want, n.fold)



SNR <- (signal.true^2 /4 + 1) / 1
SNR
# ==============================================================================
# output
# ==============================================================================
confusion_count <- function(vec_sig) {
    if(length(vec_sig) == 0){
        ONE <- 0
        TWO <- 0
        THREE <- p.true - ONE
        FOUR <- p.all - p.true - TWO
    }else{
        ONE <- sum(vec_sig <= p.true)
        TWO <- sum(vec_sig > p.true)
        THREE <- p.true - ONE
        FOUR <- p.all - p.true - TWO
    }
    return(list(ONE,TWO,THREE,FOUR))
}
# ==============================================================================
# fit DA.test
# ==============================================================================
library(data.adapt.multi.test)
out_result <- data_adapt_multi_test(Y = Y, A = A.sample.vec, n.top = p.true + 5,
                                    # n.fold = 3, parallel = FALSE) # BH on all Y
                                    n.fold = 4, parallel = FALSE) # BH on all Y
out_result
col_id_sig_final <- as.numeric(unique(unlist(lapply(out_result$top.col.name2, names))))
confusion_count(col_id_sig_final)

print(out_result)
plot(out_result)
tables <- table.data_adapt(out_result)
xtable::xtable(tables[[1]])
xtable::xtable(tables[[2]])
# ==============================================================================
# fit naive BH
# ==============================================================================
lm_out <- lm(Y ~ A.sample.vec)
B1_hat_lm <- lm_out$coefficients[2,]
names(B1_hat_lm) <- c(1:ncol(Y))

lm_summary <- summary(lm_out)
pval_lm <- sapply(lm_summary, function(x) x$coefficients[2,4])
qval_lm <- p.adjust(pval_lm, method = 'BH')
lm_sig <- which(qval_lm < .05)

confusion_count(lm_sig)


names(qval_lm) <- gsub("Response ","",names(qval_lm))
plot(head(sort(qval_lm), 100), ylim = c(0,1), ylab = 'q-value')
library(calibrate)
textxy(1:100, head(sort(qval_lm), 100), labs=names(head(sort(qval_lm), 100)), cx = 0.1, dcol = "black", m = c(-1, -4))

xtable::xtable(t(data.frame(head(sort(qval_lm), 11))))


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
    rm(list = c('b1', 'A.sample'))
    gc()
    Y <- b0 + temp1 + epsilon
    rm(list = c('b0', 'temp1'))
    gc()
    return(list(Y, A.sample.vec))
}
# ==============================================================================
# compute confusion matrix
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
    return(matrix(c(ONE,TWO,THREE,FOUR), nrow = 2, ncol = 2, byrow = TRUE))
}
# ========================================================================================
set.seed(1234)
NUM_REPEAT = 1e1
# NUM_REPEAT = 5
all_DA_Test_confuse <- list()
all_wrong_algo_confuse <- list()
all_naive_BH_confuse <- list()
for (it in 1:NUM_REPEAT) {
    print(it)
    data_here <- simulate_once(n.sim, p.all, p.true, signal.true, n.top.want, n.fold)
    Y = data_here[[1]]
    A.sample.vec = data_here[[2]]

    SNR <- (signal.true^2 /4 + 1) / 1
    SNR
    # fit DA.test
    library(adaptest)
    out_result <- adaptest(Y = Y, A = A.sample.vec, n_top = p.true + 5,
                                        n_fold = 4) # BH on all Y
    out_result
    col_id_sig_final <- as.numeric(adaptest::get_significant_biomarker(out_result))
    DA_Test_confuse <- confusion_count(col_id_sig_final)

    # fit old algorithm
    out_result_2 <- data.adapt.multi.test::data_adapt_multi_test(Y = Y, A = A.sample.vec, n.top = p.true + 5,
                                        n.fold = 4, parallel = FALSE) # BH on all Y
    out_result_2
    col_id_sig_final <- out_result_2$sig.p.FDR
    wrong_algo_confuse <- confusion_count(col_id_sig_final)

    # fit naive BH
    lm_out <- lm(Y ~ A.sample.vec)
    B1_hat_lm <- lm_out$coefficients[2,]
    names(B1_hat_lm) <- c(1:ncol(Y))

    lm_summary <- summary(lm_out)
    pval_lm <- sapply(lm_summary, function(x) x$coefficients[2,4])
    qval_lm <- p.adjust(pval_lm, method = 'BH')
    lm_sig <- which(qval_lm < .05)

    naive_BH_confuse <- confusion_count(lm_sig)

    all_DA_Test_confuse[[it]] <- DA_Test_confuse
    all_wrong_algo_confuse[[it]] <- wrong_algo_confuse
    all_naive_BH_confuse[[it]] <- naive_BH_confuse
}

Reduce('+', all_DA_Test_confuse)/length(all_DA_Test_confuse)
Reduce('+', all_wrong_algo_confuse)/length(all_wrong_algo_confuse)
Reduce('+', all_naive_BH_confuse)/length(all_naive_BH_confuse)
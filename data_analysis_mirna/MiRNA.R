# load("~/Desktop/Data adaptive CVTMLE/data_miRNA_Cliona/Sean.rdata")
load("/Users/wilsoncai/Downloads/Github/Superfund/miRNA_Cliona/data/homo/Sean.rdata")
# ==============================================================================================================================
# data preparation
# ==============================================================================================================================
exprHS <- t(exprHS)
# ==============================================================================================================================
# data adaptive target parameter
# ==============================================================================================================================
n.top.want = 30
n.top = 30
# n.top.want = 5639
# n.fold = 3
n.fold = 5

library(data.adapt.multi.test)
# test for positive effects
# data.adapt.fit <- data_adapt.multi_test(Y = exprHS, A = expose, n.top.want, n.fold, abs = FALSE, negative = FALSE)
# data.adapt.fit <- data_adapt.multi_test(Y = exprHS, A = expose, W = NULL, n.top.want	= n.top.want, n.fold = n.fold, abs = FALSE, negative = FALSE)
data.adapt.fit <- data_adapt_multi_test(Y = exprHS, A = expose, W = NULL,
                                        n.top	= n.top.want, n.fold = n.fold,
                                        absolute = FALSE, negative = FALSE,
                                        # parallel = TRUE)
                                        parallel = FALSE)


print(data.adapt.fit)
tables <- table.data_adapt(data.adapt.fit)
xtable::xtable(tables[[2]])
plot(data.adapt.fit)

colnames(exprHS)[as.numeric(colnames(tables[[1]]))]

# negative effects
data.adapt.fit <- data_adapt_multi_test(Y = exprHS, A = expose, W = NULL,
                                        n.top   = n.top.want, n.fold = n.fold,
                                        absolute = FALSE, negative = TRUE,
                                        # parallel = TRUE)
                                        parallel = FALSE)
print(data.adapt.fit)
tables <- table.data_adapt(data.adapt.fit)
xtable::xtable(tables[[2]])
plot(data.adapt.fit)

colnames(exprHS)[as.numeric(colnames(tables[[1]]))]

# =======================================================================================
# TEMP
# =======================================================================================

library(xtable)
ha <- table(data.adapt.fit)
xtable(ha, digits = c(0,0,2,4,4,1,0))

xtable(as.data.frame(ha$`top covariates`[ha$`adjusted p-values`<=.05]))

data.adapt.fit$top.index


# ==============================================================================
# fit naive BH
# ==============================================================================
lm_out <- lm(exprHS ~ expose)
B1_hat_lm <- lm_out$coefficients[2,]
names(B1_hat_lm) <- c(1:ncol(exprHS))

lm_summary <- summary(lm_out)
pval_lm <- sapply(lm_summary, function(x) x$coefficients[2,4])
qval_lm <- p.adjust(pval_lm, method = 'BH')
# which(qval_lm < .05)
# head(sort(qval_lm), 20)
# plot(head(sort(qval_lm), 100))
# plot(sort(qval_lm))

# head(B1_hat_lm[c(1:ncol(exprHS))[order(qval_lm)]], 20)

# head(sort(B1_hat_lm, decreasing = T), 20)

names(qval_lm) <- 1:length(qval_lm)
lm_sig <- which(qval_lm < .05)

confusion_count(lm_sig)


names(qval_lm) <- gsub("Response ","",names(qval_lm))
plot(head(sort(qval_lm), 100), ylim = c(0,1), ylab = 'q-value')
library(calibrate)
textxy(1:100, head(sort(qval_lm), 100), labs=names(head(sort(qval_lm), 100)), cx = 0.1, dcol = "black", m = c(-1, -4))

xtable::xtable(t(data.frame(head(sort(qval_lm), 11))))

# ==============================================================================================================================
# OPTIONAL: FDR on all covariates, without data adaptive
# ==============================================================================================================================
# test for positive effects
# data.adapt.fit <- data_adapt.multi_test(Y = exprHS, A = expose, 5639, n.fold, abs = FALSE, negative = FALSE)
data.adapt.fit <- data_adapt.multi_test(Y = exprHS, A = expose, W = NULL,
                                        n.top.want = 5639,
                                        n.fold = n.fold,
                                        abs = FALSE, negative = FALSE)
print(data.adapt.fit)
table(data.adapt.fit)
plot(data.adapt.fit)
# test for negative effects
# data.adapt.fit <- data_adapt.multi_test(Y = exprHS, A = expose, 5639, n.fold, abs = FALSE, negative = TRUE)
data.adapt.fit <- data_adapt.multi_test(Y = exprHS, A = expose, W = NULL,
                                        n.top.want = 5639,
                                        n.fold = n.fold,
                                        abs = FALSE, negative = TRUE)
print(data.adapt.fit)
table(data.adapt.fit)
plot(data.adapt.fit)

# save(list = ls(),file="no_adapt_posi.RData")
# save(list = ls(),file="no_adapt_nega.RData")
# save(list = ls(),file="adapt_posi.RData")
# save(list = ls(),file="adapt_nega.RData")

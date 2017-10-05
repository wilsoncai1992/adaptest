#' Data-adaptive test statistics for high-dimensional multiple testing
#'
#' Performs targeted minimum loss-based estimation (TMLE )of a marginal additive
#' treatment effect of a binary point treatment on an outcome. The data-adaptive
#' algorithm is used to perform variable reduction to avoid the disadvantages
#' associated with multiple testing.
#'
#' @param Y continuous or binary outcome variable
#' @param A binary treatment indicator: \code{1} = treatment, \code{0} = control
#' @param W matrix containing baseline covariates
#' @param n.top integer value for the number of candidate covariates to generate
#'        using the data-adaptive estimation algorithm
#' @param n.fold integer number of folds to be used for cross-validation
#' @param negative boolean: \code{TRUE} = test for negative effect size,
#'        \code{FALSE} = test for positive effect size
#' @param absolute boolean: \code{TRUE} = test for absolute effect size. This
#'        \code{FALSE} = test for directional effect. This overrides argument
#'        \code{negative}.
#' @param parallel boolean: \code{TRUE} = use multiple cores via \code{foreach},
#'        \code{FALSE} = single-core processing.
#'
#' @importFrom tmle tmle
#' @importFrom foreach foreach "%dopar%"
#' @importFrom parallel detectCores
#' @importFrom doParallel registerDoParallel
#' @importFrom stats lm p.adjust
#' @importFrom utils head
#' @importFrom magrittr "%>%"
#'
#' @author Wilson Cai \email{wcai@@berkeley.edu}, in collaboration with Alan E.
#'         Hubbard, with contributions from Nima S. Hejazi.
#'
#' @export data_adapt_multi_test
#'
data_adapt_multi_test <- function(Y,
                                  A,
                                  W = NULL,
                                  n.top,
                                  n.fold,
                                  absolute = FALSE,
                                  negative = FALSE,
                                  parallel = FALSE) {
  # check whether multiple cores are available if using parallel
  if (parallel) {
    if (parallel::detectCores() == 1) {
      stop("argument parallel == TRUE, but multiple cores not detected...")
    }
  }

  # use constructor function to instantiate "data_adapt" object
  data_adapt <- data_adapt(Y, A, W = NULL, n.top, n.fold, absolute = FALSE,
                           negative = FALSE, parallel = FALSE)

  # ============================================================================
  # preparation
  # ============================================================================
  n.sim <- nrow(data_adapt$Y)
  p.all <- ncol(data_adapt$Y)

  # if there is no W input, use intercept as W
  if (is.null(data_adapt$W)) {
    W <- as.matrix(rep(1, n.sim))
    data_adapt$W <- W
  }

  # ============================================================================
  # create parameter generating sample
  # ============================================================================
  # determine number of samples per fold
  sample.each.fold <- ceiling(n.sim / n.fold)

  # random index
  n.index.param.gen <- sample(head(rep(1:n.fold, each = sample.each.fold),
                                   n = n.sim))

  # number of observations in each fold
  folds.table <- table(n.index.param.gen)

  rank.all.fold <- matrix(0, nrow = n.fold, ncol = p.all)
  adapt_param_composition <- matrix(0, nrow = n.fold, ncol = n.top)
  psi.est_composition <- list()
  EIC.est_composition <- list()
  # ============================================================================
  compute.a.fold <- function(data_adapt, it0) {
    print(paste('Fold:', it0))
    chunk.as.est <- it0

    # create parameter generating data
    Y.param <- data_adapt$Y[n.index.param.gen != chunk.as.est, ]
    A.param <- data_adapt$A[n.index.param.gen != chunk.as.est]
    W.param <- data_adapt$W[n.index.param.gen != chunk.as.est, ,drop = FALSE]

    # create estimation data
    Y.est <- data_adapt$Y[n.index.param.gen == chunk.as.est, ]
    A.est <- data_adapt$A[n.index.param.gen == chunk.as.est]
    W.est <- data_adapt$W[n.index.param.gen == chunk.as.est, ,drop = FALSE]

    # generate data-adaptive target parameter
    data.adaptive.index <- data_adapt_rank(Y.param, A.param, W.param, absolute,
                                           negative)

    index.grid <- which(data.adaptive.index <= n.top)
    SL.lib <- c("SL.glm", "SL.step", "SL.glm.interaction", 'SL.gam', 'SL.earth')
    psi_list <- list()
    EIC_list <- list()
    for(it_ind in seq_along(index.grid)){
      print(index.grid[it_ind])
      tmle.estimation <- tmle(Y = Y.est[, index.grid[it_ind]], A = A.est, W = W.est,
                              Q.SL.library = SL.lib, g.SL.library = SL.lib)
      psi_list[[it_ind]] <- tmle.estimation$estimates$ATE$psi
      EIC_list[[it_ind]] <- tmle.estimation$estimates$IC$IC.ATE
    }
    psi.est <- do.call(c, psi_list)
    EIC.est <- do.call(cbind, EIC_list)

    return(list(data.adaptive.index, index.grid, psi.est, EIC.est))
  }

  # ============================================================================
  # CV
  # ============================================================================
  for (it0 in 1:n.fold) {
    list[data.adaptive.index, index.grid_here, psi.est_here, EIC.est_here] <- compute.a.fold(data_adapt, it0)
    rank.all.fold[it0,] <- data.adaptive.index
    adapt_param_composition[it0,] <- index.grid_here
    psi.est_composition[[it0]] <- psi.est_here
    EIC.est_composition[[it0]] <- EIC.est_here
  }
  psi.est_final <- do.call(rbind, psi.est_composition)
  EIC.est_final <- do.call(rbind, EIC.est_composition)
  # ============================================================================
  # statistical inference
  # ============================================================================
  Psi_output <- colMeans(psi.est_final)
  list[p.init, upper, lower, sd_by_col] <- get_pval(Psi_output, EIC.est_final, alpha=0.05)
  # adaptY_composition <- rank.all.fold[,1:n.top]
  adaptY_composition <- adapt_param_composition[,1:n.top]
  adaptY_composition <- apply(adaptY_composition, 2, function(x) table(x)/sum(table(x)))

  # compute average rank across all folds
  mean.rank <- colMeans(rank.all.fold)
  top.index <- which(rank(mean.rank) <= data_adapt$n.top)

  top.mean.rank <- mean.rank[top.index]

  # sort the top.index from the highest CV-rank to lowest
  top.index <- top.index[order(top.mean.rank)]
  top.mean.rank <- mean.rank[top.index]
  not.top.index <- setdiff(1:p.all, top.index)

  # compute proportion of existence in all folds
  is.in.top.rank <- (rank.all.fold <= data_adapt$n.top) + 0
  p.in.top.rank <- colMeans(is.in.top.rank)

  p.in.top.rank <- p.in.top.rank[top.index]
  # ============================================================================
  # perform FDR correction
  # ============================================================================
  p.final <- p.adjust(p.init, method = 'BH')

  still.sig <- p.final <= 0.05
  sig.p.FDR <- which(still.sig)

  # export covariate name for easier interpretation
  top.col.name <- adaptY_composition
  top.col.name2 <- adaptY_composition[which(still.sig)]
  # ============================================================================
  # add all newly computed statistical objects to the original data_adapt object
  # ============================================================================
  data_adapt$top.index <- top.index
  data_adapt$top.col.name <- top.col.name
  data_adapt$top.col.name2 <- top.col.name2
  # data_adapt$ATE.subset <- ATE.subset
  data_adapt$ATE.subset <- Psi_output
  data_adapt$p.init <- p.init
  data_adapt$p.final <- p.final
  data_adapt$sig.p.FDR <- sig.p.FDR
  data_adapt$top.mean.rank <- top.mean.rank
  data_adapt$p.in.top.rank <- p.in.top.rank

  # export augmented object containing the computed data-adaptive statistics
  return(data_adapt)
}

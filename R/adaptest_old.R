utils::globalVariables(c(
  "EIC_est_here", "data_adaptive_index",
  "index_grid_here", "lower", "p_value", "psi_est_here",
  "sd_by_col", "upper"
))

#' OLD Data-Adaptive Algorithm Implementation (for reference only)
#'
#' Performs targeted minimum loss-based estimation (TMLE )of a marginal additive
#' treatment effect of a binary point treatment on an outcome. The data-adaptive
#' algorithm is used to perform variable reduction to avoid the disadvantages
#' associated with multiple testing. INTERNAL USE ONLY.
#'
#' @param Y continuous or binary outcome variable
#' @param A binary treatment indicator: \code{1} = treatment, \code{0} = control
#' @param W matrix containing baseline covariates
#' @param n_top integer value for the number of candidate covariates to generate
#'  using the data-adaptive estimation algorithm
#' @param n_fold integer number of folds to be used for cross-validation
#' @param negative boolean: \code{TRUE} = test for negative effect size,
#'  \code{FALSE} = test for positive effect size
#' @param folds_vec Vector of \code{numeric} indicating the validation fold in
#'  which a given observation falls for a standard V-fold cross-validation
#'  procedure. Default is \code{NULL}, in which case a custom cross-validation
#'  procedure is used.
#' @param absolute boolean: \code{TRUE} = test for absolute effect size. This
#'  \code{FALSE} = test for directional effect. This overrides argument
#'  \code{negative}.
#' @param parameter_wrapper function
#' @param SL_lib character
#'
#' @importFrom tmle tmle
#' @importFrom stats lm p.adjust
#' @importFrom utils head
#' @importFrom magrittr "%>%"
#'
#' @author Wilson Cai \email{wcai@@berkeley.edu}, in collaboration with Alan E.
#'         Hubbard, with contributions from Nima S. Hejazi.
#'
#'
adaptest_old <- function(Y,
                         A,
                         W = NULL,
                         n_top,
                         n_fold,
                         folds_vec = NULL,
                         parameter_wrapper = adaptest::rank_DE,
                         SL_lib = c(
                           "SL.glm", "SL.step", "SL.glm.interaction",
                           "SL.gam", "SL.earth"
                         ),
                         absolute = FALSE,
                         negative = FALSE) {

  # use constructor function to instantiate "data_adapt" object
  data_adapt <- data_adapt(
    Y = Y, A = A, W = W,
    n_top = n_top,
    n_fold = n_fold,
    absolute = absolute,
    negative = negative,
    parameter_wrapper = parameter_wrapper,
    SL_lib = SL_lib
  )
  # ============================================================================
  # preparation
  # ============================================================================
  n_sim <- nrow(data_adapt$Y)
  p_all <- ncol(data_adapt$Y)

  # if there is no W input, use intercept as W
  if (is.null(data_adapt$W)) {
    W <- as.matrix(rep(1, n_sim))
    data_adapt$W <- W
  }

  # ============================================================================
  # create parameter generating sample
  # ============================================================================
  # determine number of samples per fold
  sample_each_fold <- ceiling(n_sim / n_fold)

  # Generate training set status if not given via an external input
  if (!is.null(folds_vec)) {
    index_for_folds <- folds_vec
  } else {
    # random index for cross-validation if origami not used
    index_for_folds <- sample(head(
      rep(
        seq_len(n_fold),
        each = sample_each_fold
      ),
      n = n_sim
    ))
  }

  # number of observations in each fold
  table_n_per_fold <- table(index_for_folds)

  rank_in_folds <- matrix(0, nrow = n_fold, ncol = p_all)
  adapt_param_composition <- matrix(0, nrow = n_fold, ncol = n_top)
  psi_est_composition <- list()
  EIC_est_composition <- list()
  # ============================================================================
  compute.a.fold <- function(data_adapt, it0) {
    print(paste("Fold:", it0))
    chunk_as_est <- it0

    # create parameter generating data
    Y_param <- data_adapt$Y[index_for_folds != chunk_as_est, ]
    A_param <- data_adapt$A[index_for_folds != chunk_as_est]
    W_param <- data_adapt$W[index_for_folds != chunk_as_est, , drop = FALSE]

    # create estimation data
    Y_est <- data_adapt$Y[index_for_folds == chunk_as_est, ]
    A_est <- data_adapt$A[index_for_folds == chunk_as_est]
    W_est <- data_adapt$W[index_for_folds == chunk_as_est, , drop = FALSE]

    # generate data-adaptive target parameter
    data_adaptive_index <- parameter_wrapper(
      Y = Y_param,
      A = A_param,
      W = W_param,
      absolute,
      negative
    )

    # index_grid <- which(data_adaptive_index <= n_top)
    # impose order by ranking
    df_temp <- data.frame(
      col_ind = seq_len(ncol(Y_param)),
      rank = data_adaptive_index
    )
    index_grid <- head(
      df_temp[order(df_temp$rank, decreasing = FALSE), ],
      n_top
    )[, "col_ind"]

    # estimate the parameter on estimation sample
    psi_list <- list()
    EIC_list <- list()
    for (it_index in seq_along(index_grid)) {
      # print(index_grid[it_index])
      tmle_estimation <- tmle(
        Y = Y_est[, index_grid[it_index]],
        A = A_est,
        W = W_est,
        Q.SL.library = SL_lib,
        g.SL.library = SL_lib
      )
      psi_list[[it_index]] <- tmle_estimation$estimates$ATE$psi
      EIC_list[[it_index]] <- tmle_estimation$estimates$IC$IC.ATE
    }
    psi_est <- do.call(c, psi_list)
    EIC_est <- do.call(cbind, EIC_list)

    return(list(data_adaptive_index, index_grid, psi_est, EIC_est))
  }

  # ============================================================================
  # CV
  # ============================================================================
  for (it0 in seq_len(n_fold)) {
    # list[data_adaptive_index, index_grid_here, psi_est_here, EIC_est_here] <-
    # compute.a.fold(data_adapt, it0)
    fold_out <- compute.a.fold(data_adapt, it0)
    data_adaptive_index <- fold_out[[1]]
    index_grid_here <- fold_out[[2]]
    psi_est_here <- fold_out[[3]]
    EIC_est_here <- fold_out[[4]]

    rank_in_folds[it0, ] <- data_adaptive_index
    adapt_param_composition[it0, ] <- index_grid_here
    psi_est_composition[[it0]] <- psi_est_here
    EIC_est_composition[[it0]] <- EIC_est_here
  }
  psi_est_final <- do.call(rbind, psi_est_composition)
  EIC_est_final <- do.call(rbind, EIC_est_composition)
  # ============================================================================
  # statistical inference
  # ============================================================================
  Psi_output <- colMeans(psi_est_final)
  # list[p_value, upper, lower, sd_by_col] <- get_pval(Psi_output, EIC_est_final,
  # alpha = 0.05)
  pval_out <- get_pval(Psi_output, EIC_est_final, alpha = 0.05)
  p_value <- pval_out[[1]]
  upper <- pval_out[[2]]
  lower <- pval_out[[3]]
  sd_by_col <- pval_out[[4]]

  adaptY_composition <- adapt_param_composition[, seq_len(n_top)]
  adaptY_composition <- apply(adaptY_composition, 2, function(x)
    table(x) / sum(table(x)))

  # ============================================================================
  # perform FDR correction
  # ============================================================================
  q_value <- stats::p.adjust(p_value, method = "BH")

  is_sig_q_value <- q_value <= 0.05
  significant_q <- which(is_sig_q_value)

  # export covariate name for easier interpretation
  top_colname <- adaptY_composition
  top_colname_significant_q <- adaptY_composition[which(is_sig_q_value)]
  # ============================================================================
  # compute average rank across all folds
  mean_rank <- colMeans(rank_in_folds)
  top_index <- sort(as.numeric(unique(unlist(sapply(top_colname, names)))))

  mean_rank_top <- mean_rank[top_index]

  # sort the top_index from the highest CV-rank to lowest
  top_index <- top_index[order(mean_rank_top)]
  mean_rank_top <- mean_rank[top_index]
  not_top_index <- setdiff(seq_len(p_all), top_index)

  # compute proportion of existence in all folds
  mean_rank_in_top <- (rank_in_folds <= data_adapt$n_top) + 0
  prob_in_top <- colMeans(mean_rank_in_top)

  prob_in_top <- prob_in_top[top_index]
  # ============================================================================
  # add all newly computed statistical objects to the original data_adapt object
  # ============================================================================
  data_adapt$top_index <- top_index
  data_adapt$top_colname <- top_colname
  data_adapt$top_colname_significant_q <- top_colname_significant_q
  # data_adapt$DE <- DE
  data_adapt$DE <- Psi_output
  data_adapt$p_value <- p_value
  data_adapt$q_value <- q_value
  data_adapt$significant_q <- significant_q
  data_adapt$mean_rank_top <- mean_rank_top
  data_adapt$prob_in_top <- prob_in_top

  # export augmented object containing the computed data-adaptive statistics
  return(data_adapt)
}

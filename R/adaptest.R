#' S3-Style Constructor for Data Adaptive Parameter Class
#'
#' @param Y (numeric vector) - continuous or binary biomarkers outcome variables
#' @param A (numeric vector) - binary treatment indicator: \code{1} = treatment,
#'  \code{0} = control
#' @param W (numeric vector, numeric matrix, or numeric data.frame) - matrix of
#'  baseline covariates where each column corrspond to one baseline covariate.
#'  Each row correspond to one observation
#' @param n_top (integer vector) - value for the number of candidate covariates
#'  to generate using the data-adaptive estimation algorithm.
#' @param n_fold (integer vector) - number of cross-validation folds.
#' @param absolute (logical) - whether or not to test for absolute effect size.
#'  If \code{FALSE}, test for directional effect. This overrides argument
#'  \code{negative}.
#' @param negative (logical) - whether or not to test for negative effect size.
#'  If \code{FALSE} = test for positive effect size. This is effective only when
#'  \code{absolute = FALSE}.
#' @param parameter_wrapper (function) - user-defined function that takes input
#'  (Y, A, W, absolute, negative) and outputs a (integer vector) containing
#'  ranks of biomarkers (outcome variables). For detail, please refer to the
#'  documentation for \code{rank_DE}.
#' @param SL_lib (character vector) - library of learning algorithms to be used
#'  in fitting the "Q" and "g" step of the standard TMLE procedure.
#'
#' @return \code{S3} object of class "data_adapt" for data-adaptive multiple
#'  testing.
#
data_adapt <- function(Y,
                       A,
                       W = NULL,
                       n_top,
                       n_fold,
                       absolute,
                       negative,
                       parameter_wrapper,
                       SL_lib) {
  if (!is.data.frame(Y)) {
    if (!is.matrix(Y)) {
      stop("argument Y must be a data.frame or a matrix")
    }
    Y <- as.data.frame(Y)
  }
  if (!is.vector(A)) stop("argument A must be numeric")
  if (!is.null(W)) if (!is.matrix(W)) stop("argument W must be matrix")
  if (!is.numeric(n_top)) stop("argument n_top must be numeric")
  if (!is.numeric(n_fold)) stop("argument n_fold must be numeric")
  if (!is.logical(absolute)) stop("argument absolute must be boolean/logical")
  if (!is.logical(negative)) stop("argument negative must be boolean/logical")
  if (!is.function(parameter_wrapper)) {
    stop("argument parameter_wrapper must be function")
  }
  if (!is.character(SL_lib)) stop("argument SL_lib must be character")

  # placeholders for outputs to be included when returning the data_adapt object
  top_colname <- NULL
  DE <- NULL
  p_value <- NULL
  q_value <- NULL
  significant_q <- NULL
  mean_rank_top <- NULL
  prob_in_top <- NULL
  folds <- NULL

  out <- structure(
    list(
      Y, A, W, n_top, n_fold, absolute, negative,
      parameter_wrapper, SL_lib, top_colname, DE,
      p_value, q_value, significant_q,
      mean_rank_top, prob_in_top, folds
    ),
    class = "data_adapt"
  )

  names(out) <- c(
    "Y", "A", "W", "n_top", "n_fold", "absolute", "negative",
    "parameter_wrapper", "SL_lib", "top_colname", "DE", "p_value",
    "q_value", "significant_q", "mean_rank_top", "prob_in_top",
    "folds"
  )

  # export instance of "data_adapt" for downstream use
  return(out)
}

################################################################################

#' Compute p-values based on asymptotic normality
#'
#' @param Psi_output vector containing differential expression estimates
#' @param EIC_est_final matrix where each column is the efficient influence
#'  curves of the \code{Psi_output}
#' @param alpha floating number between (0,1), significance level of the test
#'
#' @importFrom stats qnorm pnorm var
#'
#' @return \code{pval} p-values for each \code{Psi_output}
#' @return \code{upper} upper confidence bound for each \code{Psi_output}
#' @return \code{lower} lower confidence bound for each \code{Psi_output}
#' @return \code{sd_by_col} standard error of efficient influence curve for each
#'  \code{Psi_output}
#'
#' @keywords internal
#
get_pval <- function(Psi_output, EIC_est_final, alpha = 0.05) {
  n_sim <- nrow(EIC_est_final)
  var_by_col <- apply(EIC_est_final, 2, stats::var) / n_sim
  sd_by_col <- sqrt(var_by_col)
  upper <- Psi_output + abs(stats::qnorm(alpha / 2)) * sd_by_col
  lower <- Psi_output - abs(stats::qnorm(alpha / 2)) * sd_by_col

  pval <- stats::pnorm(
    abs(Psi_output / sd_by_col), mean = 0, sd = 1,
    lower.tail = FALSE
  ) * 2

  return(list(pval, upper, lower, sd_by_col))
}

################################################################################

#' Data-adaptive Statistics for High-Dimensional Multiple Testing
#'
#' Computes marginal average treatment effects of a binary point treatment on
#' multi-dimensional outcomes, adjusting for baseline covariates, using Targeted
#' Minimum Loss-Based Estimation. A data-mining algorithm is used to perform
#' biomarker selection before multiple testing to increase power.
#'
#' @param Y (numeric vector) - continuous or binary biomarkers
#'  (outcome variables)
#' @param A (numeric vector) - binary treatment indicator:
#'  \code{1} = treatment, \code{0} = control
#' @param W (numeric vector, numeric matrix, or numeric data.frame) -
#'  matrix of baseline covariates where each column corrspond to one baseline
#'  covariate. each row correspond to one observation
#' @param n_top (integer vector) - value for the number of candidate covariates
#'  to generate using the data-adaptive estimation algorithm
#' @param n_fold (integer vector) - number of cross-validation folds.
#' @param parameter_wrapper (function) - user-defined function that takes input
#'  (Y, A, W, absolute, negative) and outputs a (integer vector) containing
#'  ranks of biomarkers (outcome variables). For details, please refer to the
#'  documentation for \code{rank_DE}
#' @param SL_lib (character vector) - library of learning algorithms to be used
#'  in fitting the "Q" and "g" step of the standard TMLE procedure.
#' @param absolute (logical) - whether or not to test for absolute effect size.
#'  If \code{FALSE}, test for directional effect. This overrides argument
#'  \code{negative}.
#' @param negative (logical) - whether or not to test for negative effect size.
#'  If \code{FALSE} = test for positive effect size. This is effective only when
#'  \code{absolute = FALSE}.
#' @param p_cutoff (numeric) - p-value cutoff (default as 0.05) at and below
#'  which to be considered significant. Used in inference stage.
#' @param q_cutoff (numeric) - q-value cutoff (default as 0.05) at and below
#'  which to be considered significant. Used in multiple testing stage.
#'
#' @return S4 object of class \code{data_adapt}, sub-classed from the container
#'  class \code{SummarizedExperiment}, with the following additional slots
#'  containing data-mining selected biomarkers and their TMLE-based differential
#'  expression and inference, as well as the original call to this function (for
#'  user reference), respectively.
#' @return \code{top_index} (integer vector) - indices for the data-mining
#'  selected biomarkers
#' @return \code{top_colname} (character vector) - names for the data-mining
#'  selected biomarkers
#' @return \code{top_colname_significant_q} (character vector) - names for the
#'  data-mining selected biomarkers, which are significant after multiple
#'  testing stage
#' @return \code{DE} (numeric vector) - differential expression effect sizes for
#'  the biomarkers in \code{top_colname}
#' @return \code{p_value} (numeric vector) - p-values for the biomarkers in
#'  \code{top_colname}
#' @return \code{q_value} (numeric vector) - q-values for the biomarkers in
#'  \code{top_colname}
#' @return \code{significant_q} (integer vector) - indices of \code{top_colname}
#'  which is significant after multiple testing stage.
#' @return \code{mean_rank_top} (numeric vector) - average ranking across folds
#'  of cross-validation folds for the biomarkers in \code{top_colname}
#' @return \code{folds} (origami::folds class) - cross validation object
#'
#' @importFrom stats p.adjust
#' @importFrom utils head
#' @importFrom magrittr "%>%"
#' @importFrom origami make_folds cross_validate
#'
#' @export adaptest
#'
#' @examples
#' set.seed(1234)
#' data(simpleArray)
#' Y <- Y
#' A <- A
#'
#' adaptest(Y = Y,
#'          A = A,
#'          W = NULL,
#'          n_top = 5,
#'          n_fold = 3,
#'          SL_lib = 'SL.glm',
#'          parameter_wrapper = adaptest::rank_DE,
#'          absolute = FALSE,
#'          negative = FALSE)
#
adaptest <- function(Y,
                     A,
                     W = NULL,
                     n_top,
                     n_fold,
                     parameter_wrapper = rank_DE,
                     SL_lib = c(
                       "SL.glm", "SL.step", "SL.glm.interaction",
                       "SL.gam", "SL.earth"
                     ),
                     absolute = FALSE,
                     negative = FALSE,
                     p_cutoff = 0.05,
                     q_cutoff = 0.05) {

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
  # random index
  index_for_folds <- sample(head(
    rep(seq_len(n_fold), each = sample_each_fold),
    n = n_sim
  ))
  # number of observations in each fold
  table_n_per_fold <- table(index_for_folds)

  rank_in_folds <- matrix(0, nrow = n_fold, ncol = p_all)
  adapt_param_composition <- matrix(0, nrow = n_fold, ncol = n_top)
  psi_est_composition <- list()
  EIC_est_composition <- list()

  # origami folds
  folds <- origami::make_folds(n = n_sim, V = n_fold)
  df_all <- data.frame(Y = Y, A = A, W = W)
  cv_results <- origami::cross_validate(
    cv_fun = cv_param_est, folds = folds,
    data = df_all,
    parameter_wrapper = parameter_wrapper,
    absolute = absolute,
    negative = negative,
    n_top = n_top,
    SL_lib = SL_lib,
    Y_name = "Y",
    A_name = "A",
    W_name = "W"
  )
  # ============================================================================
  # CV
  # ============================================================================
  rank_in_folds <- matrix(
    data = cv_results$data_adaptive_index, nrow = n_fold,
    ncol = p_all, byrow = TRUE
  )
  adapt_param_composition <- matrix(
    data = cv_results$index_grid, nrow = n_fold,
    ncol = n_top, byrow = TRUE
  )
  psi_est_final <- matrix(
    data = cv_results$psi_est, nrow = n_fold,
    ncol = n_top, byrow = TRUE
  )
  EIC_est_final <- cv_results$EIC_est

  # ============================================================================
  # statistical inference
  # ============================================================================
  Psi_output <- colMeans(psi_est_final)
  # list[p_value, upper, lower, sd_by_col] <- get_pval(Psi_output, EIC_est_final,
  #                                                    alpha = 0.05)
  inference_out <- get_pval(Psi_output, EIC_est_final, alpha = p_cutoff)
  p_value <- inference_out[[1]]
  upper <- inference_out[[2]]
  lower <- inference_out[[3]]
  sd_by_col <- inference_out[[4]]

  adaptY_composition <- adapt_param_composition[, seq_len(n_top)]
  if(class(adaptY_composition) == 'integer') {
    # catch when n_top == 1; user only want top 1 gene
    adaptY_composition <- matrix(adaptY_composition, ncol = 1)
    adaptY_composition <- list(table(adaptY_composition) / sum(table(adaptY_composition)))
  }else{
    adaptY_composition <- apply(
      adaptY_composition, 2,
      function(x) table(x) / sum(table(x))
    )
  }
  # ============================================================================
  # perform FDR correction
  # ============================================================================
  q_value <- stats::p.adjust(p_value, method = "BH")
  # strict control
  # q_value <- stats::p.adjust(c(p_value, rep(1, p_all - n_top)), method = "BH")
  # q_value <- head(q_value, n_top)

  is_sig_q_value <- q_value <= q_cutoff
  significant_q <- which(is_sig_q_value)

  # export covariate name for easier interpretation
  top_colname <- adaptY_composition
  top_colname_significant_q <- adaptY_composition[which(is_sig_q_value)]

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
  data_adapt$DE <- Psi_output
  data_adapt$p_value <- p_value
  data_adapt$q_value <- q_value
  data_adapt$significant_q <- significant_q
  data_adapt$mean_rank_top <- mean_rank_top
  data_adapt$prob_in_top <- prob_in_top
  data_adapt$folds <- folds

  # export augmented object containing the computed data-adaptive statistics
  return(data_adapt)
}

################################################################################

#' Compute data-adaptive parameter estimate for a single cross-validation fold
#'
#' @param fold fold output from \code{origami}
#' @param data entire training data
#' @param Y_name string of \code{colnames} that all biomarkers share
#' @param A_name string of \code{colnames} of treatment
#' @param W_name string of \code{colnames} that all baeline covariates share
#' @param parameter_wrapper user-defined function
#' @param absolute boolean: \code{TRUE} = test for absolute effect size. This
#'  \code{FALSE} = test for directional effect. This overrides argument
#'  \code{negative}.
#' @param negative boolean: \code{TRUE} = test for negative effect size,
#'  \code{FALSE} = test for positive effect size
#' @param n_top integer value for the number of candidate covariates to generate
#'  using the data-adaptive estimation algorithm
#' @param SL_lib character of \code{SuperLearner} library
#'
#' @return \code{data_adaptive_index} (integer vector) rank for each gene
#' @return \code{index_grid} (integer matrix) gene index from rank 1 to rank K
#' @return \code{psi_est} estimand of DE for rank 1 to rank K genes
#' @return \code{EIC_est} estimand of EIC for rank 1 to rank K genes

#' @importFrom origami training validation
#' @importFrom tmle tmle
#'
cv_param_est <- function(fold,
                         data,
                         parameter_wrapper,
                         absolute,
                         negative,
                         n_top,
                         SL_lib,
                         Y_name,
                         A_name,
                         W_name) {

  # define training and validation sets based on input object of class "folds"
  param_data <- origami::training(data)
  estim_data <- origami::validation(data)

  # get param generating data
  A_param <- param_data[, grep(A_name, colnames(data))]
  Y_param <- as.matrix(param_data[, grep(Y_name, colnames(data))])
  W_param <- as.matrix(param_data[, grep(W_name, colnames(data))])
  # get estimation data
  A_estim <- estim_data[, grep(A_name, colnames(data))]
  Y_estim <- as.matrix(estim_data[, grep(Y_name, colnames(data))])
  W_estim <- as.matrix(estim_data[, grep(W_name, colnames(data))])

  # generate data-adaptive target parameter
  data_adaptive_index <- parameter_wrapper(
    Y = Y_param,
    A = A_param,
    W = W_param,
    absolute,
    negative
  )
  # index_grid <- which(data_adaptive_index <= n_top) # sorted after screening
  df_temp <- data.frame(col_ind = seq_len(ncol(Y_param)),
                        rank = data_adaptive_index) # ranked by rank
  index_grid <- head(df_temp[order(df_temp$rank, decreasing = FALSE), ],
                     n_top)[, "col_ind"]
  # estimate the parameter on estimation sample
  psi_list <- list()
  EIC_list <- list()
  for (it_index in seq_along(index_grid)) {
    tmle_estimation <- tmle::tmle(
      Y = Y_estim[, index_grid[it_index]],
      A = A_estim, W = W_estim,
      Q.SL.library = SL_lib,
      g.SL.library = SL_lib
    )
    psi_list[[it_index]] <- tmle_estimation$estimates$ATE$psi
    EIC_list[[it_index]] <- tmle_estimation$estimates$IC$IC.ATE
  }
  psi_est <- do.call(c, psi_list)
  EIC_est <- do.call(cbind, EIC_list)

  # define output object to be returned as list (for flexibility)
  out <- list(
    data_adaptive_index = data_adaptive_index,
    index_grid = index_grid,
    psi_est = psi_est,
    EIC_est = EIC_est
  )
  return(out)
}

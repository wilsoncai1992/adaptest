#' Constructor function for class data_adapt
#'
#' @param Y continuous or binary outcome variable
#' @param A binary treatment indicator: \code{1} = treatment, \code{0} = control
#' @param W vector, matrix, or data.frame containing baseline covariates
#' @param n_top integer value for the number of candidate covariates to generate
#'        using the data-adaptive estimation algorithm
#' @param n_fold integer number of folds to be used for cross-validation
#' @param negative boolean: \code{TRUE} = test for negative effect size,
#'        \code{FALSE} = test for positive effect size
#' @param absolute boolean: \code{TRUE} = test for absolute effect size. This
#'        \code{FALSE} = test for directional effect. This overrides argument
#'        \code{negative}.
#' @param parameter_wrapper function
#' @param SL_lib character
#'
#' @return S3 object of class "data_adapt" for data-adaptive multiple testing.
#'
#' @export data_adapt
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
    if(!is.matrix(Y)) {
      stop("argument Y must be a data.frame or a matrix")
    }
    Y <- as.data.frame(Y)
  }
  if (!is.vector(A)) stop("argument A must be numeric")
  if (!is.null(W)) {
    if(!is.matrix(W)) stop("argument W must be matrix")
  }
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

  out <- structure(list(Y, A, W, n_top, n_fold, absolute, negative,
                        parameter_wrapper, SL_lib,
                        top_colname, DE, p_value, q_value, significant_q,
                        mean_rank_top, prob_in_top),
                   class = "data_adapt")

  names(out) <- c("Y", "A", "W", "n_top", "n_fold", "absolute", "negative",
                  "parameter_wrapper", "SL_lib", "top_colname", "DE", "p_value",
                  "q_value", "significant_q", "mean_rank_top", "prob_in_top")

  # export instance of "data_adapt" for downstream use
  return(out)
}

################################################################################

#' Extract p-values
#'
#' @param Psi_output
#' @param EIC_est_final
#' @param alpha
#'
#' @return
#' @export
#
get_pval <- function(Psi_output, EIC_est_final, alpha=0.05) {
  n_sim <- nrow(EIC_est_final)
  var_by_col <- apply(EIC_est_final, 2, var)/n_sim
  sd_by_col <- sqrt(var_by_col)
  upper <- Psi_output + 1.96 * sd_by_col
  lower <- Psi_output - 1.96 * sd_by_col

  pval <- pnorm(abs(Psi_output/sd_by_col), mean = 0, sd = 1, lower.tail = FALSE) * 2

  return(list(pval, upper, lower, sd_by_col))
}

################################################################################

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
#' @param n_top integer value for the number of candidate covariates to generate
#'  using the data-adaptive estimation algorithm
#' @param n_fold integer number of folds to be used for cross-validation
#' @param negative boolean: \code{TRUE} = test for negative effect size,
#'  \code{FALSE} = test for positive effect size
#' @param absolute boolean: \code{TRUE} = test for absolute effect size. This
#'  \code{FALSE} = test for directional effect. This overrides argument
#'  \code{negative}.
#' @param parameter_wrapper function
#' @param SL_lib character
#'
#' @importFrom stats lm p.adjust
#' @importFrom utils head
#' @importFrom magrittr "%>%"
#' @importFrom origami make_folds cross_validate
#'
#' @author Wilson Cai \email{wcai@@berkeley.edu}, in collaboration with Alan E.
#'  Hubbard, with contributions from Nima S. Hejazi.
#'
#' @export adaptest
#
adaptest <- function(Y,
                     A,
                     W = NULL,
                     n_top,
                     n_fold,
                     parameter_wrapper = adaptest::rank_DE,
                     SL_lib = c("SL.glm", "SL.step", "SL.glm.interaction",
                                "SL.gam", "SL.earth"),
                     absolute = FALSE,
                     negative = FALSE) {

  # use constructor function to instantiate "data_adapt" object
  data_adapt <- data_adapt(Y = Y, A = A, W = W,
                           n_top = n_top,
                           n_fold = n_fold,
                           absolute = absolute,
                           negative = negative,
                           parameter_wrapper = parameter_wrapper,
                           SL_lib = SL_lib)
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
  index_for_folds <- sample(head(rep(1:n_fold, each = sample_each_fold),
                                   n = n_sim))
  # number of observations in each fold
  table_n_per_fold <- table(index_for_folds)

  rank_in_folds <- matrix(0, nrow = n_fold, ncol = p_all)
  adapt_param_composition <- matrix(0, nrow = n_fold, ncol = n_top)
  psi_est_composition <- list()
  EIC_est_composition <- list()

  # ============================================================================
  folds <- origami::make_folds(n = n_sim, V = n_fold)
  df_all <- data.frame(Y = Y, A = A, W = W)
  cv_results <- origami::cross_validate(cv_fun = cv_param_est, folds = folds,
                                        data = df_all,
                                        parameter_wrapper = parameter_wrapper,
                                        absolute = absolute,
                                        negative = negative,
                                        n_top = n_top,
                                        SL_lib = SL_lib,
                                        Y_name = 'Y',
                                        A_name = 'A',
                                        W_name = 'W')
  # ============================================================================
  # CV
  # ============================================================================
  rank_in_folds <- matrix(data = cv_results$data_adaptive_index, nrow = n_fold,
                          ncol = p_all, byrow = TRUE)
  adapt_param_composition <- matrix(data = cv_results$index_grid, nrow = n_fold,
                                    ncol = n_top, byrow = TRUE)
  psi_est_final <- matrix(data = cv_results$psi_est, nrow = n_fold,
                          ncol = n_top, byrow = TRUE)
  EIC_est_final <- cv_results$EIC_est

  # ============================================================================
  # statistical inference
  # ============================================================================
  Psi_output <- colMeans(psi_est_final)
  list[p_value, upper, lower, sd_by_col] <- get_pval(Psi_output, EIC_est_final,
                                                     alpha = 0.05)
  adaptY_composition <- adapt_param_composition[,1:n_top]
  adaptY_composition <- apply(adaptY_composition, 2,
                              function(x) table(x) / sum(table(x)))

  # ============================================================================
  # perform FDR correction
  # ============================================================================
  q_value <- p.adjust(p_value, method = 'BH')

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
  not_top_index <- setdiff(1:p_all, top_index)

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

################################################################################

#' Data-adaptive parameter estimate to be cross-validated
#'
#' @param fold ...
#' @param data ...
#' @param Y_name ...
#' @param A_name ...
#' @param W_name ...
#'
#' @importFrom origami training validation
#' @importFrom tmle tmle

cv_param_est <- function(fold, data, parameter_wrapper, absolute, negative, n_top, SL_lib, Y_name, A_name, W_name) {
  # browser()
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
  data_adaptive_index <- parameter_wrapper(Y = Y_param,
                                           A = A_param,
                                           W = W_param,
                                           absolute,
                                           negative)

  index_grid <- which(data_adaptive_index <= n_top)
  # estimate the parameter on estimation sample
  psi_list <- list()
  EIC_list <- list()
  for(it_index in seq_along(index_grid)){
    # print(index_grid[it_index])
    tmle_estimation <- tmle::tmle(Y = Y_estim[, index_grid[it_index]],
                                  A = A_estim, W = W_estim,
                                  Q.SL.library = SL_lib,
                                  g.SL.library = SL_lib)
    psi_list[[it_index]] <- tmle_estimation$estimates$ATE$psi
    EIC_list[[it_index]] <- tmle_estimation$estimates$IC$IC.ATE
  }
  psi_est <- do.call(c, psi_list)
  EIC_est <- do.call(cbind, EIC_list)

  # define output object to be returned as list (for flexibility)
  out <- list(data_adaptive_index = data_adaptive_index,
              index_grid = index_grid,
              psi_est = psi_est,
              EIC_est = EIC_est)
  return(out)
}


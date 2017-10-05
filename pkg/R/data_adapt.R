#' Constructor function for class \code{data_adapt}
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
#' @param parallel boolean: \code{TRUE} = use multiple cores via \code{foreach},
#'        \code{FALSE} = single-core processing.
#'
#' @return S3 object of class "data_adapt" for data-adaptive multiple testing.
#'
#' @export data_adapt
#'
data_adapt <- function(Y,
                       A,
                       W = NULL,
                       n_top,
                       n_fold,
                       absolute,
                       negative,
                       parallel) {
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
  if (!is.logical(parallel)) stop("argument parallel must be boolean/logical")

  # placeholders for outputs to be included when returning the data_adapt object
  top_colname <- NULL
  DE <- NULL
  p_value <- NULL
  q_value <- NULL
  significant_q <- NULL
  mean_rank_top <- NULL
  prob_in_top <- NULL

  out <- structure(list(Y, A, W, n_top, n_fold, absolute, negative, parallel,
                        top_colname, DE, p_value, q_value, significant_q,
                        mean_rank_top, prob_in_top),
                   class = "data_adapt")

  names(out) <- c("Y", "A", "W", "n_top", "n_fold", "absolute", "negative",
                  "parallel", "top_colname", "DE", "p_value", "q_value",
                  "significant_q", "mean_rank_top", "prob_in_top")

  # export instance of "data_adapt" for downstream use
  return(out)
}

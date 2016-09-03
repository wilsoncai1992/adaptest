#' Constructor function for class "data_adapt"
#'
#' @param Y continuous or binary outcome variable
#' @param A binary treatment indicator: \code{1} = treatment, \code{0} = control
#' @param W vector, matrix, or data.frame containing baseline covariates
#' @param n.top integer value for the number of candidate covariates to generate
#'              using the data-adaptive estimation algorithm
#' @param n.fold integer number of folds to be used for cross-validation
#' @param negative boolean: \code{TRUE} = test for negative effect size,
#'                          \code{FALSE} = test for positive effect size
#' @param absolute boolean: \code{TRUE} = test for absolute effect size. This
#'                          \code{FALSE} = test for directional effect. This
#'                          overrides argument \code{negative}.
#' @param parallel boolean: \code{TRUE} = use multiple cores via \code{foreach},
#'                          \code{FALSE} = single-core processing.
#' @return S3 object of class "data_adapt" for data-adaptive multiple testing.
#'
data_adapt <- function(Y, A, W = NULL, n.top, n.fold, absolute, negative,
                       parallel) {
  if (!is.numeric(n.top)) stop("argument n.top must be numeric")
  if (!is.numeric(n.fold)) stop("argument n.fold must be numeric")
  if (!is.logical(absolute)) stop("argument absolute must be boolean/logical")
  if (!is.logical(negative)) stop("argument negative must be boolean/logical")
  if (!is.logical(parallel)) stop("argument parallel must be boolean/logical")
  structure(list(Y, A, W, n.top, n.fold, absolute, negative, parallel),
            class = "data_adapt")
}

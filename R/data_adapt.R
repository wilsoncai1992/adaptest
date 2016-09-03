#' Constructor function for class "data_adapt"
#'
#' @param Y
#' @param A
#' @param W
#' @param n.top
#' @param n.fold
#' @param absolute
#' @param negative
#' @param parallel
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

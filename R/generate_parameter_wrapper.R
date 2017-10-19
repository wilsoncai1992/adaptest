#' Ranking for data-adaptive test statistics
#'
#' Performs ranking using Targeted Minimum Loss-Based Estimation. This function
#' is designed to be called inside \code{adaptest}; it should not
#' be run by itself outside of that contex.
#'
#' @param Y continuous or binary outcome variable
#' @param A binary treatment indicator: \code{1} = treatment,
#'        \code{0} = control
#' @param W vector, matrix, or data.frame containing baseline covariates
#' @param absolute boolean: \code{TRUE} = test for absolute effect size. This
#'        \code{FALSE} = test for directional effect. This overrides argument
#'        \code{negative}.
#' @param negative boolean: \code{TRUE} = test for negative effect size,
#'        \code{FALSE} = test for positive effect size
#'
#' @importFrom tmle tmle
#' @importFrom stats lm p.adjust
#'
#' @export rank_DE
#'
rank_DE <- function(Y,
                    A,
                    W,
                    absolute = FALSE,
                    negative = FALSE) {
  n_here <- nrow(Y)
  p_all <- ncol(Y)

  B1_fitted <- rep(0, p_all)

  SL_lib <- c("SL.glm", "SL.step", "SL.glm.interaction", 'SL.gam')

  for (it in 1:p_all) {
    A_fit <- A
    Y_fit <- Y[,it]
    W_fit <- as.matrix(W)
    # CASE 1: TMLE for DE effect size
    if (sum(W - as.matrix(rep(1, n_here))) != 0) {
      # if there are W
      tmle_fit <- tmle(Y = Y_fit, A = A_fit, W = W,
                          Q.SL.library = SL_lib, g.SL.library = SL_lib)
      B1_fitted[it] <- tmle_fit$estimates$ATE$psi
    } else {
      # CASE 2: OLS for faster effect size
      lm_fit <- lm(Y_fit ~ A_fit)
      B1_fitted[it] <- lm_fit$coefficients[2]
    }
  }

  # rank by absolute differential expression
  if (absolute == TRUE) {
    B1_fitted_abs <- abs(B1_fitted)
  } else {
    B1_fitted_abs <- B1_fitted
  }

  # calculate rank of each covariate
  if (negative) {
    rank_out <- rank(B1_fitted_abs)
  } else {
    rank_out <- rank(-B1_fitted_abs)
  }

  # final object to be exported by this function
  return(rank_out)
}

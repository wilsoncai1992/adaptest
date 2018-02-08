#' Compute ranking of biomarkers by sorting effect sizes
#'
#' Computes ranking of biomarkers based effect sizes, which are computed by
#' Targeted Minimum Loss-Based Estimation. This function is designed to be
#' called inside \code{adaptest}; it should not be run by itself outside of that
#' context.
#'
#' @param Y (numeric vector) - continuous or binary biomarkers outcome variables
#' @param A (numeric vector) - binary treatment indicator: \code{1} = treatment,
#'  \code{0} = control
#' @param W (numeric vector, numeric matrix, or numeric data.frame) - matrix of
#'  baseline covariates where each column corrspond to one baseline covariate.
#'  Each row correspond to one observation
#' @param absolute (logical) - whether or not to test for absolute effect size.
#'  If \code{FALSE}, test for directional effect. This overrides argument
#'  \code{negative}.
#' @param negative (logical) - whether or not to test for negative effect size.
#'  If \code{FALSE} = test for positive effect size. This is effective only when
#'  \code{absolute = FALSE}.
#'
#' @return an \code{integer vector} containing ranks of biomarkers.
#'
#' @importFrom tmle tmle
#' @importFrom stats lm as.formula coef
#'
#' @export
#
rank_DE <- function(Y,
                    A,
                    W,
                    absolute = FALSE,
                    negative = FALSE,
                    SL_lib = c("SL.glm", "SL.step", "SL.glm.interaction",
                               "SL.gam", "SL.mean")
                   ) {
  n_here <- nrow(Y)
  p_all <- ncol(Y)
  B1_fitted <- rep(0, p_all)

  # coerce into modeling variables
  A_fit <- as.numeric(A)
  W_fit <- as.matrix(W)

  for (it in seq_len(p_all)) {
    # NOTE: Y is a data.table
    Y_fit <- as.numeric(Y[, it])
    # CASE 1: TMLE for DE effect size
    if (!all(W == 1)) {
      # if there are W, then use them to adjust
      tmle_fit <- tmle::tmle(
        Y = Y_fit, A = A_fit, W = W_fit,
        Q.SL.library = SL_lib, g.SL.library = SL_lib
      )
      B1_fitted[it] <- tmle_fit$estimates$ATE$psi
    } else {
      # CASE 2: OLS for faster effect size
      lm_fit <- stats::lm(stats::as.formula("Y_fit ~ A_fit"))
      B1_fitted[it] <- stats::coef(lm_fit)[2]
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

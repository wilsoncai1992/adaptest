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
#' @param learning_library (character vector) - library of learning algorithms
#'  to be used in fitting the "Q" and "g" step of the standard TMLE procedure.
#'
#' @return an \code{integer vector} containing ranks of biomarkers.
#'
#' @importFrom tmle tmle
#' @importFrom stats lm as.formula coef
#'
#' @export
#' @examples
#' set.seed(1234)
#' data(simpleArray)
#' simulated_array <- simulated_array
#' simulated_treatment <- simulated_treatment
#' rank_DE(Y = simulated_array,
#'         A = simulated_treatment,
#'         W = rep(1, length(simulated_treatment)),
#'         absolute = FALSE,
#'         negative = FALSE)
rank_DE <- function(Y,
                    A,
                    W,
                    absolute = FALSE,
                    negative = FALSE,
                    learning_library = c(
                      "SL.glm", "SL.step",
                      "SL.glm.interaction", "SL.gam"
                    )) {
  n_here <- nrow(Y)
  p_all <- ncol(Y)

  B1_fitted <- rep(0, p_all)

  for (it in seq_len(p_all)) {
    A_fit <- A
    Y_fit <- Y[, it]
    W_fit <- as.matrix(W)
    # CASE 1: TMLE for DE effect size
    if (sum(W - as.matrix(rep(1, n_here))) != 0) {
      # if there are W
      tmle_fit <- tmle(
        Y = Y_fit, A = A_fit, W = W,
        Q.SL.library = learning_library, g.SL.library = learning_library
      )
      B1_fitted[it] <- tmle_fit$estimates$ATE$psi
    } else {
      # CASE 2: OLS for faster effect size
      lm_fit <- stats::lm(Y_fit ~ A_fit)
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

#' Compute ranking of biomarkers by sorting t-test p-values
#'
#' @param Y (numeric vector) - continuous or binary biomarkers outcome variables
#' @param A (numeric vector) - binary treatment indicator: \code{1} = treatment,
#'  \code{0} = control
#' @param W (numeric vector, numeric matrix, or numeric data.frame) - matrix of
#'  baseline covariates where each column corrspond to one baseline covariate
#'  and each row correspond to one observation.
#'
#' @return an \code{integer vector} containing ranks of biomarkers.
#'
#' @importFrom stats lm coef
#'
#' @export
#'
#' @examples
#' set.seed(1234)
#' data(simpleArray)
#' rank_ttest(Y = simulated_array,
#'            A = simulated_treatment,
#'            W = rep(1, length(A)))
#
rank_ttest <- function(Y,
                       A,
                       W) {
  n_here <- nrow(Y)
  p_all <- ncol(Y)

  lm_out <- stats::lm(Y ~ A)
  lm_summary <- summary(lm_out)
  pval_lm <- lapply(lm_summary, function(x) x$coefficients[2, 4])
  pval_lm_out <- do.call(rbind, pval_lm)

  rank_out <- rank(pval_lm_out)
  # final object to be exported by this function
  return(rank_out)
}

#' Ranking for data-adaptive test statistics
#'
#' Performs ranking using Targeted Minimum Loss-Based Estimation. This function
#' is designed to be called inside \code{data_adapt_multi_test}; it should not
#' be run by itself outside of that contex.
#'
#' @param Y.param continuous or binary outcome variable
#' @param A.param binary treatment indicator: \code{1} = treatment,
#' 																						\code{0} = control
#' @param W.param vector, matrix, or data.frame containing baseline covariates
#' @param absolute boolean: \code{TRUE} = test for absolute effect size. This
#'                          \code{FALSE} = test for directional effect. This
#'                          overrides argument \code{negative}.
#' @param negative boolean: \code{TRUE} = test for negative effect size,
#'                          \code{FALSE} = test for positive effect size
#'
#' @importFrom tmle tmle
#' @importFrom stats lm p.adjust
#'
#' @export data_adapt_rank
#'
data_adapt_rank <- function(Y.param, A.param, W.param, absolute = FALSE,
														negative = FALSE) {
	n.here <- nrow(Y.param)
	p.all <- ncol(Y.param)

	B1.fitted.all <- rep(0, p.all)

	SL.lib <- c("SL.glm", "SL.step", "SL.glm.interaction", 'SL.gam')

	for (it in 1:p.all) {
		A.fit <- A.param
		Y.fit <- Y.param[,it]
		W.fit <- as.matrix(W.param)

		# ------------------------------------------------------------------------------------
		# TMLE for effect size
		if ( !is.character(all.equal(W.param, as.matrix(rep(1, n.here)), check.attributes = FALSE)) ) {
			# if there are W
			tmle.result <- tmle(Y = Y.fit, A = A.fit, W = W.param,
													Q.SL.library = SL.lib, g.SL.library = SL.lib)
			B1.result <- tmle.result$estimates$ATE$psi
		} else {
			# ------------------------------------------------------------------------------------
			# OLS for faster effect size
			lm.result <- lm(Y.fit ~ A.fit)
			B1.result <- lm.result$coefficients[2]
		}
		# ------------------------------------------------------------------------------------
		B1.fitted.all[it] <- B1.result
	}

	if (absolute == TRUE) {
		B1.fitted.all.abs <- abs(B1.fitted.all)
	} else {
		B1.fitted.all.abs <- B1.fitted.all
	}

	# calculate rank of each covariate
	if (negative) {
		rank.out <- rank(B1.fitted.all.abs)
	} else {
		rank.out <- rank(-B1.fitted.all.abs)
	}

	# final object to be exported by this function
	return(rank.out)
}

gen.data.adaptive.rank <- function(Y.param, A.param, W.param, abs = FALSE, negative = FALSE) {
	n.here <- nrow(Y.param)
	p.all <- ncol(Y.param)

	library(tmle)
	B1.fitted.all <- rep(0, p.all)

	# SL.lib <- c("SL.glm", "SL.step", "SL.glm.interaction", 'SL.gam', 'SL.earth')
	SL.lib <- c("SL.glm", "SL.step", "SL.glm.interaction", 'SL.gam')

	for (it in 1:p.all) {
		# print(it)
		A.fit <- A.param
		Y.fit <- Y.param[,it]
		W.fit <- as.matrix(W.param)
		# ------------------------------------------------------------------------------------
		# TMLE for effect size
		if ( !all.equal(W.param, as.matrix(rep(1, n.here))) ) {
			# tmle.result <- tmle(Y = Y.fit, A = A.fit, W = as.matrix(rep(1, n.here)), Q.SL.library = SL.lib, g.SL.library = SL.lib)
			tmle.result <- tmle(Y = Y.fit, A = A.fit, W = W.param, Q.SL.library = SL.lib, g.SL.library = SL.lib)
			B1.result <- tmle.result$estimates$ATE$psi
		}else{
			# ------------------------------------------------------------------------------------
			# OLS for faster effect size
			lm.result <- lm(Y.fit ~ A.fit)
			B1.result <- lm.result$coefficients[2]
		}
		# ------------------------------------------------------------------------------------
		B1.fitted.all[it] <- B1.result
	}

	if (abs == TRUE) {
		B1.fitted.all.abs <- abs(B1.fitted.all)
	}else{
		B1.fitted.all.abs <- B1.fitted.all
	}

	# calculate rank of each covariate
	if (negative) {
		rank.out <- rank(B1.fitted.all.abs)
	}else{
		rank.out <- rank(-B1.fitted.all.abs)
	}

	to.return <- rank.out
	return(to.return)
}
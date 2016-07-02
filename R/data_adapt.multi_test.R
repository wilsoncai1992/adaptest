data_adapt.multi_test <- function(Y, A, W = NULL, n.top.want, n.fold, abs = FALSE, negative = FALSE, parallel = FALSE) {
	# ==============================================================================================================
	# preparation
	# ==============================================================================================================
	source('./gen.data.adaptive.rank.R')
	n.sim <- nrow(Y)
	p.all <- ncol(Y)

	# if there is no W input, use intercept as W
	if(is.null(W)){
		W <- as.matrix(rep(1, n.sim))
	}
	# ==============================================================================================================
	# create parameter generating sample
	# ==============================================================================================================
	# determine no of samples per fold
	sample.each.fold <- ceiling(n.sim / n.fold)
	# random index
	n.index.param.gen <- sample(head(rep(1:n.fold, each = sample.each.fold), n = n.sim))

	# number of observations in each fold
	table(n.index.param.gen)

	# Psi.hat.all.fold <- rep(0, n.fold)
	rank.all.fold <- matrix(0, nrow = n.fold, ncol = p.all)
	# ==============================================================================================================
	compute.a.fold <- function(it0) {
		print(paste('Fold:', it0))
		chunk.as.est <- it0
		# chunk.as.train <- setdiff(1:n.fold, it0)
		
		# create parameter generating data
		# airline.train <- plyr::ldply(airline.chunk[1:9])
		Y.param <- Y[n.index.param.gen != chunk.as.est,]
		A.param <- A[n.index.param.gen != chunk.as.est]
		W.param <- W[n.index.param.gen != chunk.as.est,,drop = FALSE]
		# create esimation data
		# airline.test <- plyr::ldply(airline.chunk[10])
		Y.est <- Y[n.index.param.gen == chunk.as.est,]
		A.est <- A[n.index.param.gen == chunk.as.est]
		W.est <- W[n.index.param.gen == chunk.as.est,,drop = FALSE]
		
		# ==============================================================================================================
		# data adaptive target parameter
		# ==============================================================================================================
		# data.adaptive.index <- gen.data.adaptive.rank(Y.param, A.param, abs, negative)
		data.adaptive.index <- gen.data.adaptive.rank(Y.param, A.param, W.param, abs, negative)
		return(data.adaptive.index)
	}
	
	# ==============================================================================================================
	# CV
	# ==============================================================================================================
	if (parallel) {
		library(foreach)
		library(parallel)
		library(doParallel)
		registerDoParallel(detectCores())
		rank.all.fold <- foreach(it2 = 1:n.fold, .combine = rbind) %dopar% {
			compute.a.fold(it2)
		}
	}else{
		for (it0 in 1:n.fold) {
			data.adaptive.index <- compute.a.fold(it0)
			rank.all.fold[it0,] <- data.adaptive.index
		}
	}

	
	# ==============================================================================================================
	# compute average rank across all folds
	# ==============================================================================================================
	mean.rank <- colMeans(rank.all.fold)
	top.index <- which(rank(mean.rank) <= n.top.want)

	top.mean.rank <- mean.rank[top.index]
	top.index <- top.index[order(top.mean.rank)] # sort the top.index from the highest CV-rank to lowest
	top.mean.rank <- mean.rank[top.index]
	# top.index <- which(mean.rank <= n.top.want)
	not.top.index <- setdiff(1:p.all, top.index)

	# top.rank.all.fold <- rank.all.fold[,top.index]
	# ==============================================================================================================
	# compute proportion of existence in all folds
	# ==============================================================================================================
	is.in.top.rank <- (rank.all.fold <= n.top.want) + 0
	p.in.top.rank <- colMeans(is.in.top.rank)

	p.in.top.rank <- p.in.top.rank[top.index]
	# ==============================================================================================================
	# calculate p value for top indices
	# ==============================================================================================================
	length.keep <- n.top.want
	ATE.subset <- rep(0, length.keep)
	p.val.subset <- rep(NA, length.keep)

	SL.lib <- c("SL.glm", "SL.step", "SL.glm.interaction", 'SL.gam', 'SL.earth')
	# SL.lib <- c("SL.glm", "SL.step", "SL.glm.interaction", 'SL.gam')
	for (it2 in 1:length.keep) {
		index.here <- top.index[it2]
		print(paste('estimating:', index.here))
		# tmle.estimation <- tmle(Y[,index.here], A = A, W = as.matrix(rep(1, n.sim)), Q.SL.library = SL.lib, g.SL.library = SL.lib)
		tmle.estimation <- tmle(Y[,index.here], A = A, W = W, Q.SL.library = SL.lib, g.SL.library = SL.lib)
		tmle.result.here <- tmle.estimation$estimates$ATE$psi
		tmle.p.val.here <- tmle.estimation$estimates$ATE$pvalue

		ATE.subset[it2] <- tmle.result.here
		p.val.subset[it2] <- tmle.p.val.here
	}
	# ==============================================================================================================
	# perform FDR correction
	# ==============================================================================================================
	p.init <- p.val.subset
	p.final <- p.adjust(p.init, method = 'BH')

	still.sig <- p.final <= 0.05
	still.sig.p <- top.index[still.sig]

	# ==============================================================================================================
	# boxplot.matrix(top.rank.all.fold)
	# points(1:n.top.want, y = top.mean.rank, col ='blue')
	# ==============================================================================================================
	to.return <- list(top.index, ATE.subset, p.init, p.final, still.sig.p, top.mean.rank, p.in.top.rank)
	return(to.return)
}

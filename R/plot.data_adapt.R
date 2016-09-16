#' #' Generic method of \code{plot} for objects of class \code{data_adapt}
#'
#' Customized plotting method for easily examining data-adaptive statistics
#'
#' @export plot.data_adapt
#'
plot.data_adapt <- function(adapt.fit, plot_id = c(1, 2, 3)) {
	top.index <- adapt.fit[[1]]
	ATE.subset <- adapt.fit[[2]]
	p.init <- adapt.fit[[3]]
	p.final <- adapt.fit[[4]]
	still.sig.p <- adapt.fit[[5]]
	top.mean.rank <- adapt.fit[[6]]
	p.in.top.rank <- adapt.fit[[7]]
	n.top.want <- length(top.index)

	if (1 %in% plot_id) {
		# Plot sorted average CV-rank
		plot(top.mean.rank, ylab = 'Mean CV-rank', pch = 20,
				 main = 'Mean CV-rank of selected covariates \n (Smaller the better)')

		textxy((1:n.top.want) - 0.3, top.mean.rank + 0.5, top.index, offset = .6)
	}

	if (2 %in% plot_id) {
		# plot sorted p-values, labeled with index
		temp.top.index <- top.index[order(p.final)]
		plot(sort(p.final), ylab = 'Adjusted P-value', pch = 20,
				 main = 'Adjusted p-value of selected covariates \n (Smaller the better)')
		textxy((1:n.top.want) - 0.3, sort(p.final), temp.top.index, offset = 1)
		abline(h = 0.05, lty = 2)
	}
}

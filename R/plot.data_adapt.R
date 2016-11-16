#' #' Generic method of \code{plot} for objects of class \code{data_adapt}
#'
#' Customized plotting method for easily examining data-adaptive statistics
#'
#' @param adapt.fit data-adaptive statistical object of class \code{data_adapt}
#' 									as returned by \code{data_adapt_multi_test}
#' @param plot_id number of plots to generate...unclear...
#'
#' @importFrom graphics abline plot
#'
#' @export plot.data_adapt
#'
plot.data_adapt <- function(adapt.fit, plot_id = c(1, 2, 3)) {

	top.index <- adapt.fit$top.index
	ATE.subset <- adapt.fit$ATE.subset
	p.init <- adapt.fit$p.init
	p.final <- adapt.fit$p.final
	still.sig.p <- adapt.fit$sig.p.FDR

	top.mean.rank <- adapt.fit$top.mean.rank
	p.in.top.rank <- adapt.fit$p.in.top.rank
	n.top.want <- length(top.index)

	if (1 %in% plot_id) {
		# Plot sorted average CV-rank
		plot(top.mean.rank, ylab = 'Mean CV-rank', pch = 20,
				 main = 'Mean CV-rank of selected covariates \n (Smaller the better)')

		calibrate::textxy((1:n.top.want) - 0.3, top.mean.rank + 0.5, top.index,
											offset = .6)
		abline(a = 0, b = 1, lty = 3)
	}

	if (2 %in% plot_id) {
		# plot sorted p-values, labeled with index
		temp.top.index <- top.index[order(p.final)]
		plot(sort(p.final), ylab = 'Adjusted P-value', pch = 20,
				 main = 'Adjusted p-value of selected covariates \n (Smaller the better)')
		calibrate::textxy((1:n.top.want) - 0.3, sort(p.final), temp.top.index,
											offset = 1)
		abline(h = 0.05, lty = 2)
	}
}

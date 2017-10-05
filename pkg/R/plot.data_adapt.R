#' Generic plot method for data_adapt class
#'
#' Customized plotting method for easily examining data-adaptive statistics
#'
#' @param x data-adaptive statistical object of class \code{data_adapt} as
#'       returned by \code{data_adapt_multi_test}
#' @param plot_type character vector specifying which of the two types of plots
#'        to generate: "cvrank" for a plot sorted average CV-rank, or "pvals"
#'        for a plot sorted by p-values with labels corresponding to indices
#' @param ... additional arguments passed to \code{plot} as necessary
#'
#' @importFrom graphics abline plot
#' @importFrom calibrate textxy
#'
#' @export
#'
plot.data_adapt <- function(x, ..., plot_type = c("cvrank", "pvals")) {

	top.index <- x$top.index
	ATE.subset <- x$ATE.subset
	p.init <- x$p.init
	p.final <- x$p.final
	still.sig.p <- x$sig.p.FDR

	top.mean.rank <- x$top.mean.rank
	p.in.top.rank <- x$p.in.top.rank
	n.top.want <- length(top.index)

	if ("cvrank" %in% plot_type) {
		# Plot sorted average CV-rank
		plot(top.mean.rank, ylab = 'Mean CV-rank', pch = 20,
		     main = 'Mean CV-rank of selected covariates \n (Smaller the better)')

		calibrate::textxy((1:n.top.want) - 0.3, top.mean.rank + 0.5, top.index,
											offset = .6)
		abline(a = 0, b = 1, lty = 3)
	}

	if ("pvals" %in% plot_type) {
		# plot sorted p-values, labeled with index
		# temp.top.index <- top.index[order(p.final)]
        temp.top.index <- c(1:n.top.want)[order(p.final)]
		plot(sort(p.final), ylab = 'Adjusted P-value', pch = 20,
				 main = 'Adjusted p-value of selected covariates \n (Smaller the better)')
		calibrate::textxy((1:n.top.want) - 0.3, sort(p.final), temp.top.index,
											offset = 1)
		abline(h = 0.05, lty = 2)
	}
}

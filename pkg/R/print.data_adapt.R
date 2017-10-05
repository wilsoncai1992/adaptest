#' Generic print method for data_adapt class
#'
#' Customized informative print method for examining data-adaptive statistics
#'
#' @param x data-adaptive statistical object of class \code{data_adapt} as
#'        returned by \code{data_adapt_multi_test}.
#' @param ... additional arguments passed to \code{print} as necessary
#'
#' @export
#'
print.data_adapt <- function(x, ...) {
		print('The top covariates are')
		print(get_composition(x, type = 'big')[[1]])
		print('The ATE estiamtes are')
		print(x$ATE.subset)
		print('The raw p-values are')
		print(x$p.init)
		print('The adjusted p-values are')
		print(x$p.final)
		print('The top mean CV-rank are (the smaller the better)')
		print(x$top.mean.rank)
		print(paste('The percentage of appearing in top', length(x$top.col.name),
								'are (the larger the better)'))
		print(x$p.in.top.rank*100)
        print('The covariates still significant are')
        print(x$sig.p.FDR)
		print('Their compositions are')
		print(get_composition(x, type = 'small')[[1]])
}

#' Generic method of \code{print} for objects of class \code{data_adapt}
#'
#' Customized informative print method for examining data-adaptive statistics
#'
#' @param adapt.fit data-adaptive statistical object of class \code{data_adapt}
#' 				as returned by \code{data_adapt_multi_test}.
#'
#' @export print.data_adapt
#'
print.data_adapt <- function(adapt.fit) {
		print('The top covariates are')
		print(adapt.fit$top.col.name)
		print('The ATE estiamtes are')
		print(adapt.fit$ATE.subset)
		print('The raw p-values are')
		print(adapt.fit$p.init)
		print('The adjusted p-values are')
		print(adapt.fit$p.final)
		print('The top mean CV-rank are (the smaller the better)')
		print(adapt.fit$top.mean.rank)
		print(paste('The percentage of appearing in top', length(adapt.fit$top.col.name),
								'are (the larger the better)'))
		print(adapt.fit$p.in.top.rank*100)
		print('The covariates still significant are')
		print(adapt.fit$sig.p.FDR)
}

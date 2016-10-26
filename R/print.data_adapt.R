#' Generic method of \code{print} for objects of class \code{data_adapt}
#'
#' Customized informative print method for examining data-adaptive statistics
#'
#' @param adapt.fit data-adaptive statistical object of class \code{data_adapt}
#' 									as returned by \code{data_adapt_multi_test}
#' @param shinyprint boolean indicating whether to print using HTML
#'
#' @export print.data_adapt
#'
print.data_adapt <- function(adapt.fit, shinyprint = FALSE) {
	if (!shinyprint) {
		print('The top covariates are')
		print(adapt.fit[[1]])
		print('The ATE estiamtes are')
		print(adapt.fit[[2]])
		print('The raw p-values are')
		print(adapt.fit[[3]])
		print('The adjusted p-values are')
		print(adapt.fit[[4]])
		print('The top mean CV-rank are (the smaller the better)')
		print(adapt.fit[[6]])
		print(paste('The percentage of appearing in top', length(adapt.fit[[1]]),
								'are (the larger the better)'))
		print(adapt.fit[[7]]*100)
		print('The covariates still significant are')
		print(adapt.fit[[5]])
	}
	if (shinyprint) {
		HTML(paste('<b> The covariates still significant are </b>',
							 paste(adapt.fit[[5]], collapse = ' '),
							 sep = '<br/>'
		))
	}
}

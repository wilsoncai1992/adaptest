#' Method of \code{shinyprint} for objects of class \code{data_adapt}
#'
#' Provides HTML-based printing utility for examining data-adaptive statistics
#'
#' @param adapt.fit data-adaptive statistical object of class \code{data_adapt}
#' 									as returned by \code{data_adapt_multi_test}
#'
#' @importFrom R2HTML HTML
#'
#' @export shinyprint.data_adapt
#'
shinyprint.data_adapt <- function(adapt.fit) {
	print.data_adapt(adapt.fit)
	HTML(paste('<b> The covariates still significant are </b>',
						 paste(adapt.fit[[5]], collapse = ' '),
						 sep = '<br/>'
	))
}

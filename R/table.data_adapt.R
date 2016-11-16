#' Generic method of \code{table} for objects of class \code{data_adapt}
#'
#' Customized informative tables for examining data-adaptive statistics
#'
#' @param adapt.fit data-adaptive statistical object of class \code{data_adapt}
#' 									as returned by \code{data_adapt_multi_test}
#'
#' @export table.data_adapt
#'
table.data_adapt <- function(adapt.fit, ...) {
    
	out.table <- cbind(adapt.fit$ATE.subset, adapt.fit$p.init, adapt.fit$p.final,
	                   adapt.fit$top.mean.rank, adapt.fit$p.in.top.rank * 100)
	out.table <- as.data.frame(out.table)

	# insert colnames
	out.table <- cbind(adapt.fit$top.col.name, out.table)
	names(out.table) <- c('top covariates', 'ATE estiamtes', 'raw p-values',
												'adjusted p-values', 'mean CV-rank',
												paste('% appearance in top', length(adapt.fit[[1]])))

	return(out.table)
}

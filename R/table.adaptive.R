#' @export table.adaptive
table.adaptive <- function(adapt.fit) {
	# out.table <- cbind(adapt.fit[[1]], adapt.fit[[2]], adapt.fit[[3]], adapt.fit[[4]], adapt.fit[[6]], adapt.fit[[7]]*100)
	out.table <- cbind(adapt.fit[[2]], adapt.fit[[3]], adapt.fit[[4]], adapt.fit[[6]], adapt.fit[[7]]*100)
	out.table <- as.data.frame(out.table)
	out.table[,1] <- as.integer(out.table[,1])
	out.table <- cbind(adapt.fit[[1]], out.table)
	names(out.table) <- c('top covariates', 'ATE estiamtes', 'raw p-values', 'adjusted p-values', 'mean CV-rank', paste('% appearance in top', length(adapt.fit[[1]])))

	return(out.table)
}

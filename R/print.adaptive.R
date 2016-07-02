#' @export print.adaptive
print.adaptive <- function(adapt.fit, shinyprint = FALSE) {
	if (!shinyprint) {
		print('The top covariates are')
		print(adapt.fit[[1]])
		print('The ATE estiamtes are')
		print(adapt.fit[[2]])
		print('The raw p values are')
		print(adapt.fit[[3]])
		print('The adjusted p values are')
		print(adapt.fit[[4]])
		print('The top mean CV-rank are (the smaller the better)')
		print(adapt.fit[[6]])
		print(paste('The percentage of appearing in top', length(adapt.fit[[1]]), 'are (the larger the better)'))
		print(adapt.fit[[7]]*100)
		print('The covariates still significant are')
		print(adapt.fit[[5]])
	}

	# if (shinyprint) {
	# 	HTML(paste('<b> The top covariates are </b>',
	# 						paste(adapt.fit[[1]], collapse = ' '),
	# 						'<b> The ATE estiamtes are </b>',
	# 						paste(adapt.fit[[2]], collapse = ' '),
	# 						'<b> The raw p values are </b>',
	# 						paste(adapt.fit[[3]], collapse = ' '),
	# 						'<b> The adjusted p values are </b>',
	# 						paste(adapt.fit[[4]], collapse = ' '),
	# 						'<b> The covariates still significant are </b>',
	# 						paste(adapt.fit[[5]], collapse = ' '),
	# 						'<b> The top mean CV-rank are </b>',
	# 						paste(adapt.fit[[6]], collapse = ' '),
	# 						sep = '<br/>'
	# 	))
	# }


	if (shinyprint) {
		HTML(paste('<b> The covariates still significant are </b>',
							 paste(adapt.fit[[5]], collapse = ' '),
							 sep = '<br/>'
		))
	}
}

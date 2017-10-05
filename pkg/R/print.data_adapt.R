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
    print(x$DE)
    print('The raw p-values are')
    print(x$p_value)
    print('The adjusted p-values are')
    print(x$q_value)
    print('The top mean CV-rank are (the smaller the better)')
    print(x$mean_rank_top)
    print(paste('The percentage of appearing in top', length(x$top_colname),
                'are (the larger the better)'))
    print(x$prob_in_top*100)
    print('The covariates still significant are')
    print(x$significant_q)
    print('Their compositions are')
    print(get_composition(x, type = 'small')[[1]])
}

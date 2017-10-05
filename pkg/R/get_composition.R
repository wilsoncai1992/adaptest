#' Generic method of \code{table} for objects of class \code{data_adapt}
#'
#' Customized informative tables for examining data-adaptive statistics
#'
#' @param adapt.fit data-adaptive statistical object of class \code{data_adapt}
#'              as returned by \code{data_adapt_multi_test}
#'
#' @export get_composition
#'
get_composition <- function(adapt.fit, type = 'small') {
  if (type == 'small') col.name = adapt.fit$top_colname_significant_q
  if (type == 'big') col.name = adapt.fit$top_colname

  # catch if there are no output
  if (length(col.name) == 0) return(c(NULL, NULL))

  col_id_sig_final <- as.numeric(unique(unlist(lapply(col.name, names))))
  decomposition <- matrix(0, nrow = length(col.name), ncol = length(col_id_sig_final))
  decomposition <- as.data.frame(decomposition)
  names(decomposition) <- col_id_sig_final
  for (it in 1:nrow(decomposition)) {
    decomposition[it,names(col.name[[it]])] <-col.name[[it]]
  }
  if (type == 'small') rownames(decomposition) <- adapt.fit$significant_q

  if (type == 'small') out.table <- cbind(decomposition, 'q-values' = adapt.fit$q_value[adapt.fit$significant_q])
  if (type == 'big') out.table <- cbind(decomposition, 'q-values' = NA)
  return(list(decomposition, out.table))
}

#' Title
#'
#' @param adapt.fit
#'
#' @export get_significant_biomarker
#'
#' @examples
#' # NA
get_significant_biomarker <- function(adapt.fit) {
  return(colnames(get_composition(adapt.fit, type = 'small')[[1]]))
}

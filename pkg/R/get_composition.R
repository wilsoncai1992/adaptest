#' Generic method of \code{table} for objects of class \code{data_adapt}
#'
#' Customized informative tables for examining data-adaptive statistics
#'
#' @param object data-adaptive statistical object of class \code{data_adapt}
#'              as returned by \code{adaptest}
#'
#' @export get_composition
#'
get_composition <- function(object, type = 'small') {
  if (type == 'small') col.name = object$top_colname_significant_q
  if (type == 'big') col.name = object$top_colname

  # catch if there are no output
  if (length(col.name) == 0) return(c(NULL, NULL))

  col_id_sig_final <- as.numeric(unique(unlist(lapply(col.name, names))))
  decomposition <- matrix(0, nrow = length(col.name), ncol = length(col_id_sig_final))
  decomposition <- as.data.frame(decomposition)
  names(decomposition) <- col_id_sig_final
  for (it in 1:nrow(decomposition)) {
    decomposition[it,names(col.name[[it]])] <-col.name[[it]]
  }
  if (type == 'small') rownames(decomposition) <- object$significant_q

  if (type == 'small') out.table <- cbind(decomposition, 'q-values' = object$q_value[object$significant_q])
  if (type == 'big') out.table <- cbind(decomposition, 'q-values' = NA)
  return(list(decomposition, out.table))
}

#' Title
#'
#' @param object
#'
#' @export get_significant_biomarker
#'
#' @examples
#' # NA
get_significant_biomarker <- function(object) {
  return(colnames(get_composition(object, type = 'small')[[1]]))
}

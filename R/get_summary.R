#' Decomposition tables of the data-adaptive parameter after data-mining
#'
#' Customized informative tables for examining data-adaptive statistics.
#'
#' @param object (data_adapt) - object of class \code{data_adapt} as returned by \code{adaptest}
#' @param type (character) - 'small' or 'big'. 'small' mode returns composition of
#'  data-adaptive parameters after multiple testing stage. 'big' mode returns composition of
#'  data-adaptive parameters before multiple testing stage.
#'
#' @return (numeric matrix) containing what fraction of the data-adaptive parameter comes from which biomarker in the original dataset.
#' @export
#'
get_composition <- function(object, type = "small") {
  if (type == "small") col.name <- object$top_colname_significant_q
  if (type == "big") col.name <- object$top_colname

  # catch if there are no output
  if (length(col.name) == 0) return(c(NULL, NULL))

  col_id_sig_final <- as.numeric(unique(unlist(lapply(col.name, names))))
  decomposition <- matrix(
    0, nrow = length(col.name),
    ncol = length(col_id_sig_final)
  )
  decomposition <- as.data.frame(decomposition)
  names(decomposition) <- col_id_sig_final
  for (it in 1:nrow(decomposition)) {
    decomposition[it, names(col.name[[it]])] <- col.name[[it]]
  }
  if (type == "small") rownames(decomposition) <- object$significant_q

  if (type == "small") {
    out.table <- cbind(
      decomposition,
      "q-values" = object$q_value[object$significant_q]
    )
  }
  if (type == "big") out.table <- cbind(decomposition, "q-values" = NA)
  return(list(decomposition, out.table))
}

#' Extract statistically significant biomarkers
#'
#' @param object \code{data_adapt} object
#'
#' @keywords internal
#
get_significant_biomarker <- function(object) {
  return(colnames(get_composition(object, type = "small")[[1]]))
}

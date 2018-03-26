#' Decomposition tables of the data-adaptive parameter after data-mining
#'
#' Customized informative tables for examining data-adaptive statistics.
#'
#' @param object (data_adapt) - object of class \code{data_adapt} as returned by
#'  \code{adaptest}
#' @param type (character) - 'small' or 'big'. 'small' mode returns composition
#' of data-adaptive parameters after multiple testing stage. 'big' mode returns
#' composition of data-adaptive parameters before multiple testing stage.
#'
#' @return (numeric matrix) containing what fraction of the data-adaptive
#' parameter comes from which biomarker in the original dataset.
#' @export
#' @examples
#' set.seed(1234)
#' data(simpleArray)
#' simulated_array <- simulated_array
#' simulated_treatment <- simulated_treatment
#'
#' adaptest_out <- adaptest(Y = simulated_array,
#'                          A = simulated_treatment,
#'                          W = NULL,
#'                          n_top = 5,
#'                          n_fold = 3,
#'                          learning_library = 'SL.glm',
#'                          parameter_wrapper = adaptest::rank_DE,
#'                          absolute = FALSE,
#'                          negative = FALSE)
#' get_composition(adaptest_out, type = 'small')
#
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
  for (it in seq_len(nrow(decomposition))) {
    decomposition[it, names(col.name[[it]])] <- col.name[[it]]
  }
  if (type == "small") rownames(decomposition) <- object$significant_q

  if (type == "small") {
    out.table <- cbind(
      decomposition,
      "q-values" = object$q_value[object$significant_q]
    )
  }
  if (type == "big") out.table <- cbind(decomposition, "q-values" = object$q_value)
  return(list(decomposition, out.table))
}

#' Extract statistically significant biomarkers
#' @param object \code{data_adapt} object
#' @param cutoff cut-off value for composition percentage
#'
#' @return (integer vector) of significant gene index
#'
#' @export
#'
#' @examples
#' set.seed(1234)
#' data(simpleArray)
#' simulated_array <- simulated_array
#' simulated_treatment <- simulated_treatment
#'
#' adaptest_out <- adaptest(Y = simulated_array,
#'                          A = simulated_treatment,
#'                          W = NULL,
#'                          n_top = 5,
#'                          n_fold = 3,
#'                          learning_library = 'SL.glm',
#'                          parameter_wrapper = adaptest::rank_DE,
#'                          absolute = FALSE,
#'                          negative = FALSE)
#' get_significant_biomarker(adaptest_out)
#
get_significant_biomarker <- function(object, cutoff = .5) {
  if (is.null(get_composition(object, type = "small"))) {
    return(integer()) # catch when nothing is significant
  }
  component_table <- colSums(get_composition(object, type = "small")[[1]])
  component_table <- component_table[component_table >= cutoff]
  return(as.integer(names(component_table)))
}

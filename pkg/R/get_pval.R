#' Title
#'
#' @param Psi_output
#' @param EIC.est_final
#' @param alpha
#'
#' @return
#' @export
#'
#' @examples
#' #NA
get_pval <- function(Psi_output, EIC.est_final, alpha=0.05) {
  n_sim <- nrow(EIC.est_final)
  var_by_col <- apply(EIC.est_final, 2, var)/n_sim
  sd_by_col <- sqrt(var_by_col)
  upper <- Psi_output + 1.96 * sd_by_col
  lower <- Psi_output - 1.96 * sd_by_col

  pval <- pnorm(abs(Psi_output/sd_by_col), mean = 0, sd = 1, lower.tail = FALSE) * 2

  return(list(pval, upper, lower, sd_by_col))
}

#' Method of \code{shinyprint} for objects of class \code{data_adapt}
#'
#' Provides HTML-based printing utility for examining data-adaptive statistics
#'
#' @param object data-adaptive statistical object of class \code{data_adapt}
#'        as returned by \code{adaptest}
#'
#' @importFrom R2HTML HTML
#'
#' @export shinyprint.data_adapt
#'
shinyprint.data_adapt <- function(object) {
  print.data_adapt(object)
  HTML(paste('<b> The covariates still significant are </b>',
             paste(object[[5]], collapse = ' '),
             sep = '<br/>'
  ))
}

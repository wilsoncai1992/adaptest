#' Generic plot method for data_adapt class
#'
#' Customized plotting method for easily examining data-adaptive statistics
#'
#' @param x data-adaptive statistical object of class \code{data_adapt} as
#'       returned by \code{adaptest}
#' @param plot_type character vector specifying which of the two types of plots
#'        to generate: "cvrank" for a plot sorted average CV-rank, or "pvals"
#'        for a plot sorted by p-values with labels corresponding to indices
#' @param ... additional arguments passed to \code{plot} as necessary
#'
#' @importFrom graphics abline plot
#' @importFrom calibrate textxy
#'
#' @export
#'
plot.data_adapt <- function(x, ..., plot_type = c("cvrank", "pvals")) {

  top_index <- x$top_index
  DE <- x$DE
  p_value <- x$p_value
  q_value <- x$q_value
  significant_q <- x$significant_q

  mean_rank_top <- x$mean_rank_top
  prob_in_top <- x$prob_in_top
  n_top.want <- length(top_index)

  if ("cvrank" %in% plot_type) {
    # Plot sorted average CV-rank
    plot(mean_rank_top, ylab = 'Mean CV-rank', pch = 20,
         main = 'Mean CV-rank of selected covariates \n (Smaller the better)')

    calibrate::textxy((1:n_top.want) - 0.3, mean_rank_top + 0.5, top_index,
                      offset = .6)
    abline(a = 0, b = 1, lty = 3)
  }

  if ("pvals" %in% plot_type) {
    # plot sorted p-values, labeled with index
    temp.top_index <- c(1:n_top.want)[order(q_value)]
    plot(sort(q_value),
         pch = 20,
         ylab = 'Adjusted P-value',
         main = 'Adjusted p-value of selected covariates \n (Smaller the better)'
         )
    calibrate::textxy((1:n_top.want) - 0.3, sort(q_value), temp.top_index,
                      offset = 1)
    abline(h = 0.05, lty = 2)
  }
}

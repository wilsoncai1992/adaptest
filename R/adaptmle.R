#' Constructor for class adaptmle
#'
#' @return class \code{adaptmle} object, sub-classed from SummarizedExperiment.
#'
#' @importFrom methods setClass new
#' @importClassesFrom SummarizedExperiment SummarizedExperiment
#'
#' @export .adaptmle
#' @exportClass adapTMLE
#'
#' @examples
#' library(SummarizedExperiment)
#' library(airway)
#' data(airway)
#'
#' example_adaptmle_class <- function(se, n_top = 20, n_fold = 10) {
#'     call <- match.call(expand.dots = TRUE)
#'     adaptmle <- .adaptmle(
#'          SummarizedExperiment::SummarizedExperiment(
#'             assays = SummarizedExperiment::assay(se),
#'             colData = SummarizedExperiment::colData(se)
#'          ),
#'          call = call,
#'          folds = list(),  # folds (from origami)
#'          plot_ingredients = list(),  # top_colname
#'          diff_exp = as.numeric(rep(NaN, n_top)),  # DE
#'          p_value = as.numeric(rep(NaN, n_top)),  # p_value
#'          q_value = as.numeric(rep(NaN, n_top)),  # q_value
#'          q_sig = as.numeric(rep(NaN, n_top)),  # significant_q
#'          q_sig_names = list(),  # top_colname_significant_q
#'          rank_mean = as.numeric(rep(NaN, n_top * n_fold)),  # mean_rank_top
#'          prob_top = as.numeric(rep(NaN, n_top * n_fold)),  # prob_in_top
#'          top_index = as.numeric(rep(NaN, n_top * n_fold))  # top_index
#'     )
#'     return(adaptmle)
#' }
#'
#' example_class <- example_adaptmle_class(se = airway)
#
.adaptmle <- methods::setClass(
  Class = "adapTMLE",
  slots = list(
    call = "call",
    folds = "list", # folds (from origami)
    plot_ingredients = "list", # top_colname
    diff_exp = "numeric", # DE
    p_value = "numeric", # p_value
    q_value = "numeric", # q_value
    q_sig = "numeric", # significant_q
    q_sig_names = "list", # top_colname_significant_q
    rank_mean = "numeric", # mean_rank_top
    prob_top = "numeric", # prob_in_top
    top_index = "numeric"
  ), # top_index
  contains = "SummarizedExperiment"
)

#' Accessor for results of class adaptest
#'
#' Simple accessor function to extract and assign important statistical results
#'  from the main function \code{adaptest}. INTERNAL USE ONLY.
#'
#' @param data_adapt_out S3 object of class \code{data_adapt} with analytic
#'  results from the statistical routine, as produced by \code{adaptest}.
#' @param adaptmle_in S4 object of class{adapTMLE} into which analytic results
#'  should be inserted for organizational and interfacing purposes.
#'
#' @return Called for side-effects (i.e., altering \code{adaptmle} objects).
#'
#' @keywords internal
#
get_results_adaptmle <- function(adaptmle_in, data_adapt_out) {
  stopifnot(class(data_adapt_out) == "data_adapt" &&
    class(adaptmle_in) == "adapTMLE")
  adaptmle_in@folds <- data_adapt_out$folds # from origami
  adaptmle_in@plot_ingredients <- data_adapt_out$top_colname
  adaptmle_in@diff_exp <- data_adapt_out$DE
  adaptmle_in@p_value <- data_adapt_out$p_value
  adaptmle_in@q_value <- data_adapt_out$q_value
  adaptmle_in@q_sig <- data_adapt_out$significant_q
  adaptmle_in@q_sig_names <- data_adapt_out$top_colname_significant_q
  adaptmle_in@rank_mean <- data_adapt_out$mean_rank_top
  adaptmle_in@prob_top <- data_adapt_out$prob_in_top
  adaptmle_in@top_index <- data_adapt_out$top_index
  return(adaptmle_in)
}

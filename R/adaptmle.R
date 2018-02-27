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
       slots = list(call = "call",
                    folds = "list",  # folds (from origami)
                    plot_ingredients = "list",  # top_colname
                    diff_exp = "numeric",  # DE
                    p_value = "numeric",  # p_value
                    q_value = "numeric",  # q_value
                    q_sig = "numeric",  # significant_q
                    q_sig_names = "list",  # top_colname_significant_q
                    rank_mean = "numeric",  # mean_rank_top
                    prob_top = "numeric",  # prob_in_top
                    top_index = "numeric"),  # top_index
       contains = "SummarizedExperiment"
)


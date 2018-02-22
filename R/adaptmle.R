#' Constructor for class adaptmle
#'
#' @return class \code{adaptmle} object, sub-classed from SummarizedExperiment.
#'
#' @importFrom methods setClass new
#' @importClassesFrom SummarizedExperiment SummarizedExperiment
#'
#' @export .adaptmle
#' @exportClass adapTMLE
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


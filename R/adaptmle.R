#' Constructor for class adaptmle
#'
#' @return class \code{adaptmle} object, sub-classed from SummarizedExperiment.
#'
#' @importFrom methods setClass
#' @importClassesFrom SummarizedExperiment SummarizedExperiment
#'
#' @export .adaptmle
#' @exportClass adapTMLE
#
.adaptmle <- methods::setClass(
       Class = "adapTMLE",
       slots = list(call = "call",
                    tmle_out = "data.frame",
                    top_sites = "data.frame"),
       contains = "SummarizedExperiment"
)


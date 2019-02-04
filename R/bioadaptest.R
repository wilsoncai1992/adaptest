#' Data Adaptive Multiple Testing for Computational Biology
#'
#' A thin wrapper that implements the main data-adaptive multiple hypothesis
#' testing strategy for data structures commonly found in computational biology
#' experiments, using the popular SummarizedExperiment container class.
#'
#' @param data_in An object of class \code{SummarizedExperiment}, a common
#'  container class for computational biology and bioinformatics. This object is
#'  used to construct the output object of class \code{adaptmle}.
#' @param var_int A \code{numeric} vector of binary treatment assignment whose
#'  effect on the biological units is to be assessed. The data-adpative target
#'  parameter approach finds any biological sites strongly impacted by this
#'  quantity across the observed experimental units (subjects).
#' @param cntrl_set A \code{matrix} of discrete variables representing baseline
#'  covariates that are controlled for in the estimation of the data-adaptive
#'  target parameter via targeted maximum likelihood estimation. If \code{NULL},
#'  an identity vector is generated internally.
#' @param n_top (integer vector) - value for the number of candidate covariates
#'  to generate using the data-adaptive estimation algorithm.
#' @param n_fold (integer vector) - number of cross-validation folds.
#' @param parameter_wrapper (function) - user-defined function that takes input
#'  (Y, A, W, absolute, negative) and outputs a (integer vector) containing
#'  ranks of biomarkers (outcome variables). For detail, please refer to the
#'  documentation for \code{rank_DE}.
#' @param learning_library (character vector) - library of learning algorithms
#'  to be used in fitting the "Q" and "g" step of the standard TMLE procedure.
#' @param absolute (logical) - whether or not to test for absolute effect size.
#'  If \code{FALSE}, test for directional effect. This overrides argument
#'  \code{negative}.
#' @param negative (logical) - whether or not to test for negative effect size.
#'  If \code{FALSE} = test for positive effect size. This is effective only when
#'  \code{absolute = FALSE}.
#' @param p_cutoff The minimum p-value required to evaluate a given biological
#'  unit (e.g., gene) as statistically significant.
#' @param q_cutoff The minimum p-value required to evaluate a given biological
#'  unit (e.g., gene) as statistically significant after applying a correction
#'  for multiple hypothesis testing.
#'
#' @importFrom SummarizedExperiment SummarizedExperiment assay colData rowData
#'
#' @return An object of class \code{adaptmle}, sub-classed from the popular
#'  container class \code{SummarizedExperiment}, containing information about
#'  the experiment being analyzed as well as results from applying the TMLE for
#'  the data-adaptive target parameter as produced by \code{adpatest}.
#'
#' @export
#'
#' @examples
#' library(SummarizedExperiment)
#' library(airway)
#' set.seed(5678)
#' data(airway)
#' genes_sub <- order(sample(seq_len(100)))
#' air_reduced <- airway[genes_sub, ]
#' simple_air <- cbind(air_reduced, air_reduced)
#' dex_var <- as.numeric(as.matrix(colData(simple_air))[, 3] - 1)
#' airway_out <- bioadaptest(
#'   data_in = simple_air,
#'   var_int = dex_var,
#'   cntrl_set = NULL,
#'   n_top = 5,
#'   n_fold = 2,
#'   parameter_wrapper = rank_DE
#' )
#' #
bioadaptest <- function(data_in,
                        var_int,
                        cntrl_set = NULL,
                        n_top = 25,
                        n_fold = 10,
                        parameter_wrapper = rank_DE,
                        learning_library = c("SL.mean", "SL.glm"),
                        absolute = FALSE,
                        negative = FALSE,
                        p_cutoff = 0.05,
                        q_cutoff = 0.05) {
  # ============================================================================
  # catch input and return in output object for udata_inr convenience
  # ============================================================================
  call <- match.call(expand.dots = TRUE)

  # ============================================================================
  # invoke S4 class constructor to instantiate "adapTMLE" object
  # ============================================================================
  adaptmle <- .adaptmle(
    SummarizedExperiment(
      assays = list(exps = assay(data_in)),
      rowData = rowData(data_in),
      colData = colData(data_in)
    ),
    call = call,
    folds = list(NA), # folds (from origami)
    plot_ingredients = list(NA), # top_colname
    diff_exp = as.numeric(rep(NaN, n_top)), # DE
    p_value = as.numeric(rep(NaN, n_top)), # p_value
    q_value = as.numeric(rep(NaN, n_top)), # q_value
    q_sig = as.numeric(rep(NaN, n_top)), # significant_q
    q_sig_names = list(NA), # top_colname_significant_q
    rank_mean = as.numeric(rep(NaN, n_top * n_fold)), # mean_rank_top
    prob_top = as.numeric(rep(NaN, n_top * n_fold)), # prob_in_top
    top_index = as.numeric(rep(NaN, n_top * n_fold)) # top_index
  )

  # ============================================================================
  # TMLE procedure for data-adaptive testing
  # ============================================================================
  adaptest_out <- adaptest(
    Y = adaptmle,
    A = var_int,
    W = cntrl_set,
    n_top = n_top,
    n_fold = n_fold,
    parameter_wrapper = parameter_wrapper,
    learning_library = learning_library,
    absolute = absolute,
    negative = negative,
    p_cutoff = p_cutoff,
    q_cutoff = q_cutoff
  )

  # ============================================================================
  # organize output in the adaptmle object created using accessor
  # ============================================================================
  adaptmle <- get_results_adaptmle(
    adaptmle_in = adaptmle,
    data_adapt_out = adaptest_out
  )
  return(adaptmle)
}

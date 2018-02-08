#' Data Adaptive Multiple Testing for Computational Biology
#'
#' A thin wrapper that implements the main data-adaptive multiple hypothesis
#' testing strategy for data structures commonly found in computational biology
#' experiments, using the popular SummarizedExperiment container class.
#'
#' @param data_in ...
#' @param var_int ...
#' @param cntrl_set ...
#' @param n_top ...
#' @param n_fold ...
#' @param parameter_wrapper ...
#' @param SL_lib ...
#' @param absolute ...
#' @param negative ...
#' @param p_cutoff ...
#' @param q_cutoff ...
#'
#' @return ...
#'
#' @export
#
bioadaptest <- function(data_in,
                        var_int,
                        cntrl_set = NULL,
                        n_top = 25,
                        n_fold = 10,
                        parameter_wrapper = rank_DE,
                        SL_lib = c("SL.glm", "SL.step", "SL.glm.interaction",
                                   "SL.gam", "SL.earth", "SL.mean"),
                        absolute = FALSE,
                        negative = FALSE,
                        p_cutoff = 0.05,
                        q_cutoff = 0.05
                       ) {
  # ============================================================================
  # catch input and return in output object for user convenience
  # ============================================================================
  call <- match.call(expand.dots = TRUE)

  # ============================================================================
  # invoke S4 class constructor for "adapTMLE" object
  # ============================================================================
  adaptmle <- .adaptmle(
       SummarizedExperiment::SummarizedExperiment(
          assays = list(expMeasures = assay(data_in)),
          rowData = rowData(se),
          colData = colData(se)
       ),
       call = call,
       adaptest_out = as.data.frame(matrix(NA, 10, 10)),
       top_table = as.data.frame(matrix(NA, 10, 10))
  )

  # ============================================================================
  # wrangle input data structures such that we can feed them to main function
  # ============================================================================
  W <- cntrl_set
  A <- var_int
  Y <- data.table::as.data.table(t(SummarizedExperiment::assay(data_in)))

  # ============================================================================
  # TMLE procedure for data-adaptive testing
  # ============================================================================
  adaptest_out <- adaptest(Y = Y,
                           A = A,
                           W = W,
                           n_top = n_top,
                           n_fold = n_fold,
                           parameter_wrapper = parameter_wrapper,
                           SL_lib = SL_lib,
                           absolute = absolute,
                           negative = negative,
                           p_cutoff = p_cutoff,
                           q_cutoff = q_cutoff
                          )

}


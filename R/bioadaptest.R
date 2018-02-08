bioadaptest <- function(se,
                        var_int,
                        parallel = TRUE,
                        bppar_type = NULL,
                        future_param = NULL,
                        subj_ids = NULL,
                        g_lib = c("SL.glm", "SL.randomForest", "SL.nnet",
                                  "SL.polymars", "SL.mean"),
                        Q_lib = c("SL.glm", "SL.randomForest", "SL.nnet",
                                  "SL.mean")
                        ) {
  # ============================================================================
  # catch input and return in output object for user convenience
  # ============================================================================
  call <- match.call(expand.dots = TRUE)

  # ============================================================================
  # invoke S4 class constructor for "adapTMLE" object
  # ============================================================================
  adaptmle <- .adaptmle(
       SummarizedExperiment(
          assays = list(expMeasures = assay(se)),
          rowData = rowData(se),
          colData = colData(se)
       ),
       call = call,
       tmle_out = as.data.frame(matrix(NA, 10, 10)),
       top_table = as.data.frame(matrix(NA, 10, 10))
  )

  # ============================================================================
  # set up parallelization based on input
  # ============================================================================
  doFuture::registerDoFuture()
  if (parallel == TRUE) {
    if (!is.null(future_param)) {
      set_future_param <- parse(text = paste0("future", "::", future_param))
      future::plan(eval(set_future_param))
    } else {
      future::plan(future::multiprocess)
    }
  } else if (parallel == FALSE) {
    warning(paste("Sequential evaluation is strongly discouraged.",
                  "\n Proceed with caution."))
    future::plan(future::sequential)
  }
  if (!is.null(bppar_type)) {
    bp_type <- eval(parse(text = paste0("BiocParallel", "::",
                                        bppar_type, "()")))
  } else {
    bp_type <- BiocParallel::DoparParam()
  }
  BiocParallel::bpprogressbar(bp_type) <- TRUE
  BiocParallel::register(bp_type, default = TRUE)

  # ============================================================================
  # TMLE procedure for data-adaptive testing
  # ============================================================================


}

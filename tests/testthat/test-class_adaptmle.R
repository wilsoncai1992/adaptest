context("adaptmle S4 class.")
library(airway)
data(airway)

################################################################################
## SETUP TESTS #################################################################
################################################################################

example_adaptmle_class <- function(se, n_top = 20, n_fold = 10) {
    call <- match.call(expand.dots = TRUE)
    adaptmle <- .adaptmle(
         SummarizedExperiment::SummarizedExperiment(
            assays = SummarizedExperiment::assay(se),
            colData = SummarizedExperiment::colData(se)
         ),
         call = call,
         folds = list(),  # folds (from origami)
         plot_ingredients = list(),  # top_colname
         diff_exp = as.numeric(rep(NaN, n_top)),  # DE
         p_value = as.numeric(rep(NaN, n_top)),  # p_value
         q_value = as.numeric(rep(NaN, n_top)),  # q_value
         q_sig = as.numeric(rep(NaN, n_top)),  # significant_q
         q_sig_names = list(),  # top_colname_significant_q
         rank_mean = as.numeric(rep(NaN, n_top * n_fold)),  # mean_rank_top
         prob_top = as.numeric(rep(NaN, n_top * n_fold)),  # prob_in_top
         top_index = as.numeric(rep(NaN, n_top * n_fold))  # top_index
    )
    return(adaptmle)
}

adaptmle <- example_adaptmle_class(se = airway)

################################################################################
## BEGIN TESTS #################################################################
################################################################################

test_that("adaptmle object is of class type S4", {
  expect_equivalent(typeof(adaptmle), "S4")
})

test_that("adaptmle object is of appropriate class", {
  expect_equivalent(class(adaptmle), "adapTMLE")
})

test_that("adaptmle is a subclass of SummarizedExperiment", {
  expect_is(adaptmle, "SummarizedExperiment")
})

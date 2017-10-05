library(adaptest)
context("Creating data_adapt objects")

test_that("data_adapt fails when invoked improperly", {

  expect_error( data_adapt(Y = c(1, 2, 3, 4, 5, 6),
                           A = c(rep(0, 3), rep(1, 3)),
                           W = NULL,
                           n.top = TRUE,
                           n.fold = 3,
                           absolute = TRUE,
                           negative = TRUE,
                           parallel = FALSE)
              )

})

library(testthat)
library(adaptest)

Sys.setenv(R_TESTS = "")
test_check("adaptest")

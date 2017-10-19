.onAttach <- function(...) {
  packageStartupMessage("adaptest: Data-adaptive test statistics for multiple testing in high-dimensional settings")
  packageStartupMessage("Version: ",
                        utils::packageDescription("adaptest")$Version)
}

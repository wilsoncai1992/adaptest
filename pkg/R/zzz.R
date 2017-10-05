.onAttach <- function(...) {
  packageStartupMessage("DA.Test: Data-adaptive test statistics for multiple testing in high-dimensional settings")
  packageStartupMessage("Version: ",
                        utils::packageDescription("DA.Test")$Version)
}

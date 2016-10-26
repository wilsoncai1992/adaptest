.onAttach <- function(...) {
  packageStartupMessage("data.adapt.multi.test: Data-adaptive test statistics for multiple testing in high-dimensional settings")
  packageStartupMessage("Version: ",
                        utils::packageDescription("data.adapt.multi.test")$Version)
}

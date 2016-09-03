.onAttach <- function(...) {
  packageStartupMessage("data.adapt.multi.test: Data-adaptive test statistics for high-dimensional multiple testing")
  packageStartupMessage("Version: ",
                        utils::packageDescription("data.adapt.multi.test")$Version)
}

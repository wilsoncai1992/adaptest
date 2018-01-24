.onAttach <- function(...) {
  packageStartupMessage(paste(
    "adaptest: Data-adaptive statistics for",
    "\n high-dimensional multiple testing"
  ))
  packageStartupMessage(
    "Version: ",
    utils::packageDescription("adaptest")$Version
  )
}

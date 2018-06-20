.onAttach <- function(...) {
  packageStartupMessage(paste0(
    "adaptest v", utils::packageDescription("adaptest")$Version,
    ": Data-Adaptive Statistics for High-Dimensional Multiple Testing"
  ))
}

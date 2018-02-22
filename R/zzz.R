.onAttach <- function(...) {
    packageStartupMessage(paste0(
        "adaptest v", utils::packageDescription("adaptest")$Version,
        ": Data-adaptive statistics for high-dimensional multiple testing"
    ))
}

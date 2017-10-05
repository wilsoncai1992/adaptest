# utils::globalVariables(c("new"))
# #' Constructor for class data.adapt
# #'
# #' @return class \code{data.adapt} object, sub-classed from SummarizedExperiment.
# #'
# #' @importClassesFrom SummarizedExperiment SummarizedExperiment
# #'
# #' @export .data.adapt
# #' @exportClass bioTMLE
# #'
# #' @examples
# #' library(someData)
# #' data(illuminaData)
# #' library(SummarizedExperiment)
# #'
# #' example_data.adapt_class <- function(se) {
# #'
# #'     call <- match.call(expand.dots = TRUE)
# #'     data.adapt <- .data.adapt(
# #'           SummarizedExperiment(
# #'              assays = assay(se),
# #'              rowData = rowData(se),
# #'              colData = colData(se)
# #'           ),
# #'           call = call,
# #'           tmleOut = as.data.frame(matrix(NA, 10, 10)),
# #'           modtestOut = as.data.frame(matrix(NA, 10, 10)),
# #'           topTable = as.data.frame(matrix(NA, 10, 10))
# #'     )
# #'     return(data.adapt)
# #' }
# #'
# #' example_class <- example_data.adapt_class(se = illuminaData)
# #'
# .data.adapt <- setClass(
#        Class = "data.adapt",
#        slots = list(call = "call",
#                     tmleOut = "data.frame",
#                     modtestOut = "data.frame",
#                     topTable = "data.frame"),
#        contains = "SummarizedExperiment"
# )

#' Create a new BenchDesign
#'
#' Initializes a new BenchDesign object for benchmarking methods.
#'
#' @param ... named set of \code{BDMethod} objects.
#' @param methods named set of \code{BDMethod} objects as a list. (default = NULL)
#' @param data optional data.frame or other list object to be
#'        used in the benchmark. (default = NULL)
#'
#' @return
#' \code{BenchDesign} object.
#'
#' @examples
#' ## with no input
#' bd <- BenchDesign()
#'
#' ## with toy data.frame
#' df <- data.frame(x1 = rnorm(20), y1 = rnorm(20))
#' bd <- BenchDesign(data = df)
#'
#' @rdname BenchDesign-class
#' @export
#' @author Patrick Kimes
setGeneric("BenchDesign",
           valueClass = "BenchDesign",
           function(..., methods = NULL, data = NULL) standardGeneric("BenchDesign"))

.BenchDesign <- function(..., methods, data) {
    ml <- list(...)
    if (!is.null(methods))
        ml <- c(ml, methods)
    if (length(ml) == 0)
        ml <- list()
    
    if (!is.null(data)) {
        data <- new("BDData", data = data,
                    type = ifelse(is(data, "character"), "hash", "data"))
    }

    new("BenchDesign", methods = ml, data = data)
}

.BenchDesign.sb <- function(methods, data) {
    if (!is.null(data))
        message("Note: Ignoring specified data and using SummarizedBenchmark object.")
    bd <- methods@BenchDesign
    if (is.null(bd))
        bd <- BenchDesign()
    bd
}

#' @rdname BenchDesign-class
setMethod("BenchDesign", signature(methods = "ANY", data = "ANY"), .BenchDesign)
setMethod("BenchDesign", signature(methods = "SummarizedBenchmark", data = "ANY"), .BenchDesign.sb)




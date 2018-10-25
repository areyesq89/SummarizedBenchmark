#' Create a new BenchDesign object
#'
#' Initializes a new BenchDesign object for benchmarking methods.
#'
#' @param ... named set of \code{BDMethod} objects and/or unnamed
#'        \code{BenchDesign} objects. Only the methods of any \code{BenchDesign}
#'        object will be used, and the data slot of the objects will be
#'        ignored.
#' @param methods named set of \code{BDMethod} objects and/or unnamed
#'        \code{BenchDesign} objects as a list. (default = NULL)
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
#' @name BenchDesign
#' @export
#' @author Patrick Kimes
NULL

.BenchDesign <- function(..., methods, data) {
    ## add shortcut for just a single SummarizedBenchmark object (want to return w/ data)
    ml <- list(...)
    if (length(ml) == 1 && is.null(methods) && is.null(data) && is(ml[[1]], "SummarizedBenchmark"))
        return(BenchDesign(methods = ml[[1]]))
    
    bdml <- BDMethodList(..., x = methods)
    if (!is.null(data) && !is(data, "BDData")) {
        data <- new("BDData", data = data,
                    type = ifelse(is(data, "character"), "md5hash", "data"))
    }
    new("BenchDesign", methods = bdml, data = data)
}

#' @rdname BenchDesign
#' @exportMethod "BenchDesign"
setMethod("BenchDesign", signature(methods = "ANY", data = "ANY"), .BenchDesign)

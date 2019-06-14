#' Create a new BenchDesign object
#'
#' @description
#' Initializes a new BenchDesign object of benchmarking methods and data.
#'
#' The BenchDesign class serves as the core container
#' for methods and data used for benchmarking in the SummarizedBenchmark package. The object
#' can be initialized with a list of methods ot be benchmarked, a default benchmarking data set,
#' both or neither. Methods must be passed to the constructor as \code{BDMethod} or \code{BDMethodList}
#' objects.
#'
#' The constructor can also be used to access the BenchDesign stored in a SummarizedBenchmark
#' object.
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
#' ## with data - data must be a named argument
#' datadf <- data.frame(pval = runif(20), x1 = rnorm(20))
#' bd <- BenchDesign(data = datadf)
#'
#' ## with two methods and data
#' method_bh <- BDMethod(stats::p.adjust, params = rlang::quos(p = pval, method = "BH"))
#' method_bf <- BDMethod(stats::p.adjust, params = rlang::quos(p = pval, method = "bonferroni"))
#' bd <- BenchDesign(bh = method_bh, bonf = method_bf,
#'                   data = datadf)
#'
#' ## with BDMethodList and data
#' bdml <- BDMethodList(bh = method_bh, bonf = method_bf)
#' bd <- BenchDesign(methods = bdml, data = datadf)
#' 
#' @seealso \code{\link{BenchDesign-class}}, \code{\link{BDMethod}}, \code{\link{BDMethodList}}
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

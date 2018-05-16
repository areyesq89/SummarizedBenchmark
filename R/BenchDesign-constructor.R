#' Create a new BDMethod
#'
#' Initializes a new BenchDesign object for benchmarking methods.
#'
#' @param f a function to be benchmarked
#' @param params a list of quosures specifying function parameters
#' @param post a list of functions to be applied to the output of \code{f}
#' @param meta a list of meta data
#' 
#' @return
#' BDMethod object
#'
#' @rdname BDMethod-class
#' @importFrom rlang enquo
#' @export
#' @author Patrick Kimes
BDMethod <- function(f, params = rlang::quos(), post = NULL, meta = NULL) {
    qf <- rlang::enquo(f)
    if (is(post, "function")) {
        post <- list("default" = post)
    }
    new("BDMethod", f = f, qf = qf, params = params, post = post, meta = meta)
}


## Compare BDMethod meta data
#'
#' Simple comparison of two BDMethod objects based on
#' meta data.
#' 
#' @param bdm1 a \code{BDMethod} object
#' @param bdm2 a \code{BDMethod} object
#'
#' @return
#' logical value indicating whether two objects produced same
#' meta data.
#' 
#' @importFrom dplyr all_equal
#' @author Patrick Kimes
compareBDMethod <- function(bdm1, bdm2) {
    dplyr::all_equal(tidyBDMethod(bdm1), tidyBDMethod(bdm2))
}



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
        ml <- NULL

    if (!is.null(data)) {
        data <- new("BDData", data = data,
                    type = ifelse(is(data, "character"), "hash", "data"))
    }

    new("BenchDesign", methods = ml, data = data)
}

#' @rdname BenchDesign-class
setMethod("BenchDesign", signature(methods = "ANY", data = "ANY"), .BenchDesign)


.BenchDesign.sb <- function(methods, data) {
    if (!is.null(data))
        message("Note: Ignoring specified data and using SummarizedBenchmark object.")
    bd <- methods@BenchDesign
    if (is.null(bd))
        bd <- BenchDesign()
    bd
}

#' @rdname BenchDesign-class
setMethod("BenchDesign", signature(methods = "SummarizedBenchmark", data = "ANY"), .BenchDesign.sb)




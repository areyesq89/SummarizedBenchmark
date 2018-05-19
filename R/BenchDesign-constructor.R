#' Create a new BenchDesign
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
#' @importFrom rlang flatten
#' @rdname BenchDesign-class
#' @export
#' @author Patrick Kimes
setGeneric("BenchDesign",
           valueClass = "BenchDesign",
           function(..., methods = NULL, data = NULL) standardGeneric("BenchDesign"))

.BenchDesign <- function(..., methods, data) {
    ml <- list(...)
    if (is.null(names(ml)) || any(names(ml) == "ml"))
    if (!is.null(methods))
        ml <- c(ml, methods)
    if (length(ml) == 0)
        ml <- list()

    ## allow shortcut of calling BenchDesing on SB w/out specifying methods=
    if (length(ml) == 1 && is(ml[[1]], "SummarizedBenchmark"))
        return(BenchDesign(methods = ml[[1]]))

    ## flatten any BD or SB objects
    ml_is_bd <- unlist(lapply(ml, is, "BenchDesign"))
    ml_is_bdm <- unlist(lapply(ml, is, "BDMethod"))
    ml_is_sb <- unlist(lapply(ml, is, "SummarizedBenchmark"))
    stopifnot(ml_is_bd | ml_is_bdm | ml_is_sb)
    if (any(ml_is_bd)) {
        ml[ml_is_bd] <- lapply(ml[ml_is_bd],  slot, name = "methods")
        ml <- rlang::flatten(ml)
    }
    if (any(ml_is_sb)) {
        ml[ml_is_sb] <- lapply(ml[ml_is_sb], function(x) BenchDesign(methods = x))
        return(BenchDesign(methods = ml, data = data))
    }

    if (!is.null(data)) {
        data <- new("BDData", data = data,
                    type = ifelse(is(data, "character"), "md5hash", "data"))
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

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
setGeneric("BDMethod",
           function(f, params = rlang::quos(), post = NULL, meta = NULL) standardGeneric("BDMethod"))

.BDMethod.quo <- function(f, params = rlang::quos(), post = NULL, meta = NULL) {
    fc <- rlang::get_expr(f)
    f <- rlang::eval_tidy(f)
    if (is(post, "function")) {
        post <- list("default" = post)
    }
    new("BDMethod", f = f, fc = fc, params = params, post = post, meta = meta)
}

.BDMethod.fun <- function(f, params = rlang::quos(), post = NULL, meta = NULL) {
    fc <- substitute(f)
    if (is(post, "function")) {
        post <- list("default" = post)
    }
    new("BDMethod", f = f, fc = fc, params = params, post = post, meta = meta)
}

#' @rdname BDMethod-class
setMethod("BDMethod", signature(f = "quosure"), .BDMethod.quo)
setMethod("BDMethod", signature(f = "ANY"), .BDMethod.fun)

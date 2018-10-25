#' Create a new BDMethod object
#'
#' Initializes a new BenchDesign method (BDMethod) object for benchmarking methods.
#'
#' @param x main method function or function quosure. Alternative, may be a
#'        BDMethodList or BenchDesign object from which the BDMethod should be
#'        extracted.
#' @param params list of quosures specifying function parameters.
#'        (default = \code{rlang::quos()})
#' @param post list of functions to be applied to the output of \code{x}.
#'        (default = NULL)
#' @param meta list of metadata. (default = NULL)
#' @param i integer index or character name of BDMethod in BDMethodList or
#'        BenchDesign object.
#' @param ... other parameters.
#' 
#' @return
#' BDMethod object
#'
#' @examples
#' BDMethod(x = base::mean)
#'
#' BDMethod(x = function(x) { x^2 }, post = base::sqrt,
#'          meta = list(note = "simple example"))
#'
#' bd <- BenchDesign(method1 = BDMethod(x = base::mean))
#' BDMethod(bd, "method1")
#' 
#' @name BDMethod
#' @import rlang
#' @export
NULL

.BDMethod.quo <- function(x, params, post, meta, ...) {
    fc <- rlang::get_expr(x)
    f <- rlang::eval_tidy(x)
    if (is(post, "function")) {
        post <- list("default" = post)
    }
    new("BDMethod", f = f, fc = fc, params = params, post = post, meta = meta)
}

.BDMethod.fun <- function(x, params, post, meta, ...) {
    fc <- substitute(x)
    if (is(post, "function")) {
        post <- list("default" = post)
    }
    new("BDMethod", f = x, fc = fc, params = params, post = post, meta = meta)
}


#' @rdname BDMethod
#' @export
setMethod("BDMethod", signature(x = "quosure"), .BDMethod.quo)

#' @rdname BDMethod
#' @export
setMethod("BDMethod", signature(x = "function"), .BDMethod.fun)

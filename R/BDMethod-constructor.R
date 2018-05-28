#' Create a new BDMethod
#'
#' Initializes a new BenchDesign method (BDMethod) object for benchmarking methods.
#'
#' @param x main method function or function quosure
#' @param params list of quosures specifying function parameters.
#'        (default = code{rlang::quos()})
#' @param post list of functions to be applied to the output of \code{x}.
#'        (default = NULL)
#' @param meta list of meta data. (default = NULL)
#' @param ... other parameters.
#' 
#' @return
#' BDMethod object
#'
#' @name BDMethod
#' @importFrom rlang enquo
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

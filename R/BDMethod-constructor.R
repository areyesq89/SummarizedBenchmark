#' Create a new BDMethod object
#'
#' @description
#' Initializes a new BenchDesign method object for benchmarking.
#'
#' New \code{\link[=BDMethod-class]{BDMethod}} objects are typically not directly constructed as they
#' have limited use outside of \code{\link[=BenchDesign-class]{BenchDesign}} objects. Instead,
#' methods in a \code{\link[=BenchDesign-class]{BenchDesign}} object are more commonly created, modified or
#' removed using
#' function calls on the \code{\link[=BenchDesign-class]{BenchDesign}}, e.g. using \code{\link{addMethod}} to
#' add a new method object.
#'
#' The constructor can also be used to access \code{\link[=BDMethod-class]{BDMethod}} objects stored in
#' \code{\link[=BDMethodList-class]{BDMethodList}} and \code{\link[=BenchDesign-class]{BenchDesign}} objects.
#'
#' @param x main method function or function quosure. Alternative, may be a
#'        \code{\link[=BDMethodList-class]{BDMethodList}} or
#'        \code{\link[=BenchDesign-class]{BenchDesign}} object from which
#'        the \code{\link[=BDMethod-class]{BDMethod}} should be
#'        extracted.
#' @param params list of quosures specifying function parameters.
#'        (default = \code{rlang::quos()})
#' @param post list of functions to be applied to the output of \code{x}.
#'        (default = NULL)
#' @param meta list of metadata. (default = NULL)
#' @param i integer index or character name of \code{\link[=BDMethod-class]{BDMethod}} in
#'        \code{\link[=BDMethodList-class]{BDMethodList}} or
#'        \code{\link[=BenchDesign-class]{BenchDesign}} object.
#' @param ... other parameters.
#' 
#' @return
#' BDMethod object
#'
#' @examples
#' ## create a simple BDMethod
#' bdm1 <- BDMethod(x = base::mean)
#'
#' ## create a more complex BDMethod
#' bdm2 <- BDMethod(x = function(x) { x^2 }, post = base::sqrt,
#'                  meta = list(note = "simple example"))
#'
#' ## construct a BenchDesign with the BDMethod objects
#' bd <- BenchDesign(method1 = bdm1, method2 = bdm2)
#'
#' ## access a BDMethod in the BenchDesign
#' BDMethod(bd, "method1")
#' 
#' @seealso \code{\link{BDMethod-class}}, \code{\link{BenchDesign}}, \code{\link{BDMethodList}}
#' @name BDMethod
#' @import rlang
#' @export
#' @author Patrick Kimes
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

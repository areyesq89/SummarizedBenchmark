#' Create a new BDMethod
#'
#' Initializes a new BenchDesign method (BDMethod) object for benchmarking methods.
#'
#' @param x a function or BenchDesign object
#' @param params a list of quosures specifying function parameters.
#'        (default = code{rlang::quos()})
#' @param post a list of functions to be applied to the output of \code{x}.
#'        (default = NULL)
#' @param meta a list of meta data. (default = NULL)
#' @param i an numeric or characeter index. (default = 1)
#' 
#' @return
#' BDMethod object
#'
#' @rdname BDMethod-class
#' @importFrom rlang enquo
#' @export
setGeneric("BDMethod", valueClass = "BDMethod",
           function(x, params = rlang::quos(), post = NULL, meta = NULL, ...) standardGeneric("BDMethod"))

.BDMethod.quo <- function(x, params, post, meta, ...) {
    fc <- rlang::get_expr(x)
    f <- rlang::eval_tidy(x)
    if (is(post, "function")) {
        post <- list("default" = post)
    }
    new("BDMethod", f = f, fc = fc, params = params, post = post, meta = meta)
}

.BDMethod.bdml <- function(x, i = 1) {
    stopifnot(is(i, "character") || is(i, "numeric"))
    x[[i]]
}

.BDMethod.bd <- function(x, i = 1) {
    BDMethod(x@methods, i = i)
}

.BDMethod.sb <- function(x, i = 1) {
    BDMethod(BenchDesign(x), i = i)
}

.BDMethod.fun <- function(x, params, post, meta, ...) {
    fc <- substitute(x)
    if (is(post, "function")) {
        post <- list("default" = post)
    }
    new("BDMethod", f = x, fc = fc, params = params, post = post, meta = meta)
}


#' @rdname BDMethod-class
setMethod("BDMethod", signature(x = "quosure"), .BDMethod.quo)
setMethod("BDMethod", signature(x = "BDMethodList"), .BDMethod.bdml)
setMethod("BDMethod", signature(x = "BenchDesign"), .BDMethod.bd)
setMethod("BDMethod", signature(x = "SummarizedBenchmark"), .BDMethod.sb)
setMethod("BDMethod", signature(x = "function"), .BDMethod.fun)


#' @rdname BenchDesign-class
#' @export
setGeneric("BDMethod<-", 
           function(x, i, ..., value) standardGeneric("BDMethod<-"))

#' @rdname BenchDesign-class
#' @exportMethod "BDMethod<-"
setReplaceMethod("BDMethod",
                 signature(x = "BenchDesign", i = "character", value = "BDMethod"),
                 function (x, i, value) {
                     x@methods[[i]] <- value
                     x
                 })

#' @rdname BenchDesign-class
#' @exportMethod "BDMethod<-"
setReplaceMethod("BDMethod",
                 signature(x = "BenchDesign", i = "character", value = "NULL"),
                 function (x, i, value) {
                     x@methods[[i]] <- value
                     x
                 })


#' @rdname BDMethodList-class
#' @exportMethod "BDMethod<-"
setReplaceMethod("BDMethod",
                 signature(x = "BDMethodList", i = "character", value = "BDMethod"),
                 function (x, i, value) {
                     x[[i]] <- value
                     x
                 })

#' @rdname BDMethodList-class
#' @exportMethod "BDMethod<-"
setReplaceMethod("BDMethod",
                 signature(x = "BDMethodList", i = "character", value = "NULL"),
                 function (x, i, value) {
                     x[[i]] <- value
                     x
                 })

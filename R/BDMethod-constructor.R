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
#' @param id a index or character string. (default = 1)
#' 
#' @return
#' BDMethod object
#'
#' @rdname BDMethod-class
#' @importFrom rlang enquo
#' @export
#' @author Patrick Kimes
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

.BDMethod.bd <- function(x, id = 1) {
    stopifnot(is(id, "character") || is(id, "numeric"))
    x@methods[[id]]
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
setMethod("BDMethod", signature(x = "BenchDesign"), .BDMethod.bd)
setMethod("BDMethod", signature(x = "ANY"), .BDMethod.fun)


#' Create a new BDMethodList
#'
#' Initialized a new SimpleList of BenchDesign method (BDMethod) objects.
#'
#' @param ... a named list of BDMethod objects
#' 
#' @return
#' BDMethodList object
#'
#' @examples
#' BDMethodList()
#' 
#' @importFrom rlang flatten
#' @rdname BDMethodList-class
#' @export
#' @author Patrick Kimes
setGeneric("BDMethodList", valueClass = "BDMethodList",
           function(..., object = NULL) standardGeneric("BDMethodList"))


.BDMethodList <- function(..., object) {
    ml <- list(...)
    if (!is.null(object))
        ml <- c(ml, object)
    if (length(ml) == 0)
        ml <- list()

    ## allow shortcut for calling on list, SB, BD w/out specifying object=
    if (length(ml) == 1) {
        if (is.list(ml[[1]]) || is(ml[[1]], "List"))
            ml <- ml[[1]]
        else if (is(ml[[1]], "BenchDesign") || is(ml[[1]], "SummarizedBenchmark"))
            return(BDMethodList(object = ml[[1]]))
    }

    
    
    ## extract any BD objects and flatten any BDML objects
    ml_is_bd <- unlist(lapply(ml, is, "BenchDesign"))
    ml_is_bdm <- unlist(lapply(ml, is, "BDMethod"))
    ml_is_list <- unlist(lapply(ml, is, "list")) | unlist(lapply(ml, is, "List"))
    if (any(!(ml_is_bd | ml_is_bdm | ml_is_list)))
        stop("When specifying more than one object, only specify BenchDesign, BDMethod or list/List objects.")

    if (any(ml_is_bd)) {
        ml[ml_is_bd] <- lapply(ml[ml_is_bd], BDMethodList)
    }
    if (any(ml_is_bd | ml_is_bdml)) {
        ml[ml_is_bd | ml_is_list] <- lapply(ml[ml_is_bd | ml_is_list], as.list)
        ml <- rlang::flatten(ml)
    }
    
    if (length(ml) > 0 && (is.null(names(ml)) ||
                           any(!nzchar(names(ml))) ||
                           any(duplicated(names(ml)))))
        stop("Methods must be specified with unique names.")
    
    new("BDMethodList", S4Vectors::SimpleList(ml))
}

.BDMethodList.bd <- function(..., object) {
    object@methods
}

.BDMethodList.sb <- function(..., object) {
    BDMethodList(object = BenchDesign(methods = object))
}

#' @rdname BDMethodList-class
setMethod("BDMethodList", signature(object = "ANY"), .BDMethodList)
setMethod("BDMethodList", signature(object = "BenchDesign"), .BDMethodList.bd)
setMethod("BDMethodList", signature(object = "SummarizedBenchmark"), .BDMethodList.sb)


#' @rdname BDMethodList-class
setAs("list", "BDMethodList", function(from) {
    new("BDMethodList", as(from, "SimpleList"))
})
setAs("List", "BDMethodList", function(from) {
    new("BDMethodList", as(from, "SimpleList"))
})


#' @rdname BenchDesign-class
#' @export
setGeneric("BDMethodList<-", 
           function(x, i, ..., value) standardGeneric("BDMethodList<-"))

#' @rdname BenchDesign-class
#' @exportMethod "BDMethodList<-"
setReplaceMethod("BDMethodList",
                 signature(x = "BenchDesign", i = "missing", value = "BDMethodList"),
                 function (x, value) {
                     x@methods <- value
                     x
                 })

setReplaceMethod("BDMethodList",
                 signature(x = "BenchDesign", i = "character", value = "BDMethod"),
                 function (x, i, value) {
                     x@methods[[i]] <- value
                     x
                 })


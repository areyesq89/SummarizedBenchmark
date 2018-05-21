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
#' @rdname BenchDesign-class
#' @export
#' @author Patrick Kimes
setGeneric("BenchDesign",
           valueClass = "BenchDesign",
           function(..., methods = NULL, data = NULL) standardGeneric("BenchDesign"))


.BenchDesign <- function(..., methods, data) {
    ## add shortcut for just a single SummarizedBenchmark object (want to return w/ data)
    ml <- list(...)
    if (length(ml) == 1 && is.null(methods) && is.null(data) && is(ml[[1]], "SummarizedBenchmark"))
        return(BenchDesign(methods = ml[[1]]))
    
    bdml <- BDMethodList(..., object = methods)
    if (!is.null(data) && !is(data, "BDData")) {
        data <- new("BDData", data = data,
                    type = ifelse(is(data, "character"), "md5hash", "data"))
    }
    new("BenchDesign", methods = bdml, data = data)
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


#' @rdname BenchDesign-class
#' @export
setGeneric("BDMethodList<-", 
           function(x, ..., value) standardGeneric("BDMethodList<-"))

#' @rdname BenchDesign-class
#' @exportMethod "BDMethodList<-"
setReplaceMethod("BDMethodList",
                 signature(x = "BenchDesign", value = "BDMethodList"),
                 function (x, value) {
                     x@methods <- value
                     x
                 })


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


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
    if (any(ml_is_bd | ml_is_list)) {
        ml[ml_is_bd | ml_is_list] <- lapply(ml[ml_is_bd | ml_is_list], as.list)
        ml <- rlang::flatten(ml)
    }
    
    if (length(ml) > 0 && (is.null(names(ml)) ||
                           any(!nzchar(names(ml))) ||
                           any(duplicated(names(ml)))))
        stop("Methods must be specified with unique names.")

    if (is(ml, "List"))
        return(new("BDMethodList", ml))
    else 
        return(new("BDMethodList", S4Vectors::SimpleList(ml)))
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
#' @name coerce
setAs("list", "BDMethodList", function(from) {
    new("BDMethodList", as(from, "SimpleList"))
})
setAs("List", "BDMethodList", function(from) {
    new("BDMethodList", as(from, "SimpleList"))
})



#' Create a new BDMethodList object
#'
#' @description
#' Initializes a new SimpleList of BenchDesign method (\code{\link[=BDMethod-class]{BDMethod}}) objects.
#' 
#' Similar to \code{\link[=BDMethod-class]{BDMethod}} objects, \code{\link[=BDMethodList-class]{BDMethodList}}
#' typically do not need to be
#' directly constructed. Because the list of methods is only useful as art of a
#' \code{\link[=BenchDesign-class]{BenchDesign}} object, it is more common to simply manipulate the
#' list of methods through calls to the corresponding \code{\link[=BenchDesign-class]{BenchDesign}}, e.g.
#' \code{\link{addMethod}} to add a new method to the list.
#'
#' The constructor can also be used to access the \code{\link[=BDMethodList-class]{BDMethodList}} list
#' of methods in a \code{\link[=BenchDesign-class]{BenchDesign}} object.
#' 
#' @param ... a named list of \code{\link[=BDMethod-class]{BDMethod}} objects
#' @param x a \code{\link[=BenchDesign-class]{BenchDesign}} or
#'        \code{\link[=SummarizedBenchmark-class]{SummarizedBenchmark}} object to extract
#'        the list of methods from. (default = NULL)
#' 
#' @return
#' BDMethodList object
#'
#' @examples
#' ## construct an empty list
#' bdml <- BDMethodList()
#' 
#' ## construct a list with BDMethod objects
#' bdml <- BDMethodList(m_method = BDMethod(base::mean),
#'                      s_method = BDMethod(function(x) { x^2 }))
#' bdml
#'
#' ## construct a BenchDesign with a BDMethodList
#' bd <- BenchDesign(methods = bdml)
#'
#' ## access the BDMethodList in the BenchDesign
#' BDMethodList(bd)
#'
#' @seealso \code{\link{BDMethodList-class}}, \code{\link{BenchDesign}}, \code{\link{BDMethod}}
#' @name BDMethodList
#' @importFrom rlang flatten
#' @importFrom S4Vectors SimpleList
#' @export
#' @author Patrick Kimes
NULL

.BDMethodList <- function(..., x) {
    ml <- list(...)
    if (!is.null(x))
        ml <- c(ml, x)
    if (length(ml) == 0)
        ml <- list()
    
    ## allow shortcut for calling on list, SB, BD w/out specifying x=
    if (length(ml) == 1) {
        if (is.list(ml[[1]]) || is(ml[[1]], "List")) {
            ml <- ml[[1]]
        } else if (is(ml[[1]], "BenchDesign") || is(ml[[1]], "SummarizedBenchmark")) {
            return(BDMethodList(x = ml[[1]]))
        }
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

#' @rdname BDMethodList
setMethod("BDMethodList", signature(x = "ANY"), .BDMethodList)

#' @rdname BDMethodList
#' @name coerce
setAs("list", "BDMethodList", function(from) {
    new("BDMethodList", as(from, "SimpleList"))
})
setAs("List", "BDMethodList", function(from) {
    new("BDMethodList", as(from, "SimpleList"))
})


#' Add a new method to a BenchDesign
#'
#' @param b BenchDesign object
#' @param blabel name for the method
#' @param bfunc function to be benchmarked
#' @param bpost optional post-processing function that takes output of func,
#'         e.g. a getter method, ignored if NULL (default = NULL)
#' @param ... named parameter, value pairs to be used with func
#' 
#' @export
#' @author Patrick Kimes
addMethod <- function(b, blabel, bfunc, bpost = NULL, ...) {
    UseMethod("addMethod")
}

#' @export
addMethod.BenchDesign <- function(b, blabel, bfunc, bpost = NULL, ...) {
    ## capture input
    qf <- enquo(bfunc)
    qd <- quos(...)
    qp <- enquo(bpost)
    
    ## add to bench
    b$methods[[blabel]] <- list(func = qf, dparams = qd,
                                post = qp, params = quo(list()))
    b
}

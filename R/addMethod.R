#' Add new method to BenchDesign object
#'
#' @param b BenchDesign object
#' @param blabel character name for the method
#' @param bfunc primary function to be benchmarked
#' @param bpost optional post-processing function that takes output of
#'        \code{bfunc} e.g. a getter method, ignored if NULL
#'        (default = NULL)
#' @param ... named \code{parameter = value} pairs to be passed to
#'        \code{func}
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
    b$methods[[blabel]] <- list(func = qf, dparams = qd, post = qp)
    b
}

#' Remove method from BenchDesign object
#'
#' This function takes a BenchDesign object and the name of a method
#' already defined in the object, and returns a reduced BenchDesign
#' object with the specified method removed.
#'
#' @param b BenchDesign object.
#' @param blabel Character name of method to be modified.
#'
#' @return
#' Modified BenchDesign object.
#'
#' @examples
#' ## with toy data.frame
#' df <- data.frame(pval = rnorm(100))
#' bd <- BenchDesign(df)
#'
#' ## add methods
#' bd <- addBMethod(bd, blabel = "bonf", bfunc = p.adjust,
#'                  p = pval, method = "bonferroni")
#' bd <- addBMethod(bd, blabel = "BH", bfunc = p.adjust,
#'                  p = pval, method = "BH")
#'
#' ## remove methods
#' bd <- dropBMethod(bd, blabel = "bonf")
#' 
#' @md
#' @export
#' @author Patrick Kimes
dropBMethod <- function(b, blabel) {
    UseMethod("dropBMethod")
}

#' @export
dropBMethod.BenchDesign <- function(b, blabel) {
    ## verify that method definition already exists
    if(!(blabel %in% names(b$methods))) {
        stop("Specified method is not defined in BenchDesign.")
    }

    b$methods[blabel] <- NULL
    return(b)
}


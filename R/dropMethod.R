#' Remove method from BenchDesign object
#'
#' This function takes a BenchDesign object and the name of a method
#' already defined in the object, and returns a reduced BenchDesign
#' object with the specified method removed.
#'
#' @param bd BenchDesign object.
#' @param label Character name of method to be modified.
#'
#' @return
#' Modified BenchDesign object.
#'
#' @examples
#' ## with toy data.frame
#' df <- data.frame(pval = rnorm(100))
#' bench <- BenchDesign(df)
#'
#' ## add methods
#' bench <- addMethod(bench, label = "bonf", func = p.adjust,
#'                    params = rlang::quos(p = pval, method = "bonferroni"))
#' bench <- addMethod(bench, label = "BH", func = p.adjust,
#'                    params = rlang::quos(p = pval, method = "BH"))
#'
#' ## remove methods
#' bench <- dropMethod(bench, label = "bonf")
#' 
#' @md
#' @export
#' @author Patrick Kimes
dropMethod <- function(bd, label) {
    UseMethod("dropMethod")
}

#' @export
dropMethod.BenchDesign <- function(bd, label) {
    ## verify that method definition already exists
    if(!(label %in% names(bd$methods))) {
        stop("Specified method is not defined in BenchDesign.")
    }

    bd$methods[label] <- NULL
    return(bd)
}


#' Remove method from BenchDesign object
#'
#' @description
#' Takes a \code{\link[=BenchDesign-class]{BenchDesign}} object and the name of an
#' existing method
#' and returns a reduced \code{\link[=BenchDesign-class]{BenchDesign}} object with the
#' method removed.
#'
#' @param bd \code{\link[=BenchDesign-class]{BenchDesign}} object.
#' @param label Character name of method.
#'
#' @return
#' Modified \code{\link[=BenchDesign-class]{BenchDesign}} object with
#' specified method dropped.
#'
#' @examples
#' ## empty BenchDesign
#' bench <- BenchDesign()
#'
#' ## add methods
#' bench <- addMethod(bench, label = "bonf", func = p.adjust,
#'                    params = rlang::quos(p = pval, method = "bonferroni"))
#' bench <- addMethod(bench, label = "BH", func = p.adjust,
#'                    params = rlang::quos(p = pval, method = "BH"))
#' BDMethodList(bench)
#' 
#' ## remove methods
#' bench <- dropMethod(bench, label = "bonf")
#' BDMethodList(bench)
#' 
#' @seealso \code{\link{modifyMethod}}, \code{\link{expandMethod}}, \code{\link{addMethod}}
#' @md
#' @export
#' @author Patrick Kimes
dropMethod <- function(bd, label) {
    UseMethod("dropMethod")
}

#' @export
dropMethod.BenchDesign <- function(bd, label) {
    ## verify that method definition already exists
    if(!(label %in% names(bd@methods))) {
        stop("Specified method is not defined in BenchDesign.")
    }

    bd@methods[[label]] <- NULL
    return(bd)
}


#' Pretty print methods in a BenchDesign object
#'
#' Print out details about a method included in
#' the BenchDesign. The `printMethods` function is just a wrapper
#' to call `printMethod` on all methods in the BenchDesign.
#' 
#' @param bd BenchDesign object.
#' @param n name of a method in the BenchDesign to show.
#'
#' @return
#' Brief description is returned to console.
#'
#' @examples
#' ## create empty BenchDesign
#' bench <- BenchDesign()
#'
#' ## currently no methods
#' printMethods(bench)
#'
#' ## add method
#' bench <- addMethod(bench, label = "method_a", p.adjust)
#' bench <- addMethod(bench, label = "method_b", qvalue::qvalue)
#'
#' ## show a single method
#' printMethod(bench, "method_a")
#'
#' ## show all methods
#' printMethods(bench)
#'
#' @seealso \code{\link{BDMethod-class}}, \code{\link{BenchDesign-class}}
#' @md
#' @importFrom stringr str_pad
#' @export
#' @author Patrick Kimes
printMethod <- function(bd, n = NULL) {
    if (is.null(n))
        n <- names(bd@methods)[1]
    stopifnot(n %in% names(bd@methods))
    
    cat(stringr::str_pad(paste0(n, " "), 60, "right", pad = "-"), "\n")
    show(bd@methods[[n]])
}


#' @rdname printMethod
#' @export
printMethods <- function(bd) {
    for (n in names(bd@methods)) {
        printMethod(bd, n)
    }
}

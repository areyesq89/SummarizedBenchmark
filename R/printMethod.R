#' Pretty print methods in a BenchDesign
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
#' @md
#' @import rlang
#' @importFrom stringr str_trunc str_pad
#' @export
#' @author Patrick Kimes
printMethod <- function(bd, n) {
    stopifnot(n %in% names(bd$methods))

    cat(stringr::str_pad(paste0(n, " "), 60, "right", pad = "-"), "\n")

    m <- bd$methods[[n]]
    cat("func:\n")
    cat("    ", quo_text(m$func), "\n")

    cat("post:\n")
    cat("    ", quo_text(m$post), "\n")

    cat("meta:\n")
    if (!is.null(m$meta)) {
        mmeta <- eval_tidy(m$meta)
        meta_n <- names(mmeta)
        meta_q <- sapply(mmeta, quo_text)
        for (i in seq(meta_n))
            cat(stringr::str_trunc(paste("    ", meta_n[i], ":", meta_q[i]), 60), "\n")
    } else {
        cat("     none\n")
    }
    
    cat("parameters:\n")
    if (length(m$dparams) > 0) {
        param_n <- names(m$dparams)
        param_q <- sapply(m$dparams, quo_text)
        for (i in seq(param_n))
            cat(stringr::str_trunc(paste("    ", param_n[i], ":", param_q[i]), 60), "\n")
    } else {
        cat("     none\n")
    }
}


#' @rdname printMethod
#' @export
#' @author Patrick Kimes
printMethods <- function(bd) {
    for (n in names(bd$methods)) {
        printMethod(bd, n)
    }
}

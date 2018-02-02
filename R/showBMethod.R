#' Pretty print methods in a BenchDesign
#'
#' Print out details about a method included in
#' the BenchDesign. The `showBMethods` function is just a wrapper
#' to call `showBMethod` on all methods in the BenchDesign.
#' 
#' @param b BenchDesign object.
#' @param n name of a method in the BenchDesign to show.
#'
#' @return
#' Brief description is returned to console.
#'
#' @examples
#' \dontrun{
#' bd <- BenchDesign()
#' showBMethods(bd)
#' }
#' 
#' @md
#' @import rlang
#' @importFrom stringr str_trunc str_pad
#' @export
#' @author Patrick Kimes
showBMethod <- function(b, n) {
    stopifnot(n %in% names(b$methods))

    cat(stringr::str_pad(paste0(n, " "), 60, "right", pad = "-"), "\n")

    m <- b$methods[[n]]
    cat("bfunc:\n")
    cat("    ", quo_text(m$func), "\n")

    cat("bpost:\n")
    cat("    ", quo_text(m$post), "\n")

    cat("bmeta:\n")
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


#' @rdname showBMethod
#' @export
#' @author Patrick Kimes
showBMethods <- function(b) {
    for (n in names(b$methods)) {
        showBMethod(b, n)
    }
}

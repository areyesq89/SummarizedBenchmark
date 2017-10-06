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

    dn <- names(m$dparams)
    d <- sapply(m$dparams, quo_name)
    cat("parameters:\n")
    for (i in seq(dn))
        cat("    ", dn[i], ":", d[i], "\n")
}


#' @rdname showBMethod
#' @export
#' @author Patrick Kimes
showBMethods <- function(b) {
    for (n in names(b$methods)) {
        showBMethod(b, n)
    }
}

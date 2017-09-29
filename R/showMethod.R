#' Pretty print a single method
#'
#' Easy method for printing out details about a method included in
#' the BenchDesign. The `showMethods` function is just a wrapper
#' to call `showMethod` on all methods for in the BenchDesign 
#' 
#' @param b BenchDesign object
#' @param n name of a method in the BenchDesign
#'
#' @return
#' Brief description is returned to console.
#' 
#' @md
#' @export
#' @author Patrick Kimes
showMethod <- function(b, n) {
    stopifnot(n %in% names(b$methods))

    cat(stringr::str_pad(paste0(n, " "), 60, "right", pad = "-"), "\n")

    m <- b$methods[[n]]
    cat("core:\n")
    cat("    ", quo_text(m$func), "\n")

    cat("post-processing:\n")
    cat("    ", quo_text(m$post), "\n")

    dn <- names(m$dparams)
    d <- sapply(m$dparams, quo_name)
    cat("baseline:\n")
    for (i in seq(dn))
        cat("    ", dn[i], ":", d[i], "\n")

    pn <- names(m$params)
    p <- sapply(m$params, quo_name)
    cat("variable:\n")
    for (i in seq(pn))
        cat("    ", pn[i], ":", p[i], "\n")

}


#' Pretty print all method
#'
#' @param b BenchDesign object
#'
#' @rdname showMethod
#' @export
#' @author Patrick Kimes
showMethods <- function(b) {
    for (n in names(b$methods)) {
        showMethod(b, n)
    }
}

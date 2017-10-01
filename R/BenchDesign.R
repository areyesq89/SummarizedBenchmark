#' Create a new BenchDesign
#'
#' Initializes a new BenchDesign object for benchmarking methods.
#'
#' @param bdata data.frame or other list object to be used for the
#'        benchmark (default = NULL)
#'
#' @return
#' a BenchDesign object
#'
#' @import rlang
#' @export
#' @author Patrick Kimes
BenchDesign <- function(bdata = NULL) {
    b <- structure(list(bdata = bdata,
                        methods = list()),
                   class = "BenchDesign")
    b
}


#' Reports whether x is a BenchDesign object
#' 
#' @param x object to test
#' 
#' @keywords internal
#' @export
#' @author Patrick Kimes
is.BenchDesign <- function(x) {
    inherits(x, "BenchDesign")
}


#' Pretty print BenchDesign
#' 
#' @param x a BenchDesign
#' @param ... other parameters
#'
#' @keywords internal
#' @importFrom stringr str_pad
#' @export
#' @author Patrick Kimes
print.BenchDesign <- function(x, ...) {
    cat(stringr::str_pad("BenchDesign object ", 60, "right", "-"), "\n")
    cat("\n")
    cat("benchmark data:\n")
    if (!is.null(x$bdata)) {
        cat("    class: ", class(x$bdata), "\n")
        cat("    names: ", paste(names(x$bdata), collapse=", "), "\n")
    } else {
        cat("    NULL\n")
    }
    cat("\n")
    cat("benchmark methods:\n")
    if (length(x$methods) < 1) {
        cat("    none\n")
    } else { 
        m1 <- max(nchar(names(head(x$methods))))
        m2 <- max(nchar(sapply(head(x$methods), function(x) { quo_text(x$func) })))
        for (n in names(head(x$methods))) {
            p1 <- stringr::str_pad(n, m1 + 1, "left", " ")
            p2 <- stringr::str_pad(quo_text(x$methods[[n]]$func), m2 + 1, "left", " ")
            cat("    method: ", p1, "; func: ", p2, "\n", sep="")
        }
    }
}

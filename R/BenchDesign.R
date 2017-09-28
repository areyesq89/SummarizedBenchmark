#' create a new BenchDesign
#'
#' Initializes a new BenchDesign object.
#'
#' @param bdata data to be used for benchmarking methods
#'        (default = NULL)
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
#' @param x object to test
#' @keywords internal
#' @export
is.BenchDesign <- function(x) {
    inherits(x, "BenchDesign")
}


#' Pretty print BenchDesign
#'
#' @param x a BenchDesign
#'
#' @importFrom stringr str_pad
#' @export 
print.BenchDesign <- function(x) {
    cat(stringr::str_pad("BenchDesign object ", 60, "right", "-"), "\n")
    cat("\n")
    cat("benchmark data:\n")
    if (!is.null(x$bdata)) {
        cat("    class: ", class(x$bdata), "\n")
        cat("    names: ", paste(names(x$bdatba), collapse=", "), "\n")
    } else {
        cat("    NULL\n")
    }
    cat("\n")
    cat("benchmark methods:\n")
    if (length(x$methods) < 1) {
        cat("    none\n")
    } else { 
        for (n in names(head(x$methods))) {
            cat("    method: ", n, "; func: ",
                quo_text(b$methods[[n]]$func), "\n")
        }
    }
}

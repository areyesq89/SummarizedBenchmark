.show.BenchDesign <- function(object) {
    cat(stringr::str_pad("BenchDesign ", 60, "right", "-"), "\n")
    cat("  benchmark data:\n")
    if (!is.null(object@data)) {
        dtype <- object@data@type
        cat("    type:", object@data@type, "\n")
        if (dtype == "data")
            cat("    names:", paste(names(object@data@data), collapse=", "), "\n")
        if (dtype == "md5hash")
            cat("    MD5 hash:", stringr::str_trunc(object@data@data, 50), "\n")
    } else {
        cat("    NULL\n")
    }
    cat("  benchmark methods:\n")
    if (length(object@methods) < 1) {
        cat("    none\n")
    } else {
        max_c <- 20
        m1 <- max(nchar(names(head(object@methods))))
        m1 <- min(m1, max_c)
        m2 <- max(nchar(sapply(head(object@methods), function(x) { rlang::quo_text(x@fc) })))
        m2 <- min(m2, max_c)
        for (n in names(head(object@methods, 5))) {
            p1 <- stringr::str_pad(stringr::str_trunc(n, max_c), m1 + 1, "left", " ")
            p2 <- stringr::str_pad(stringr::str_trunc(gsub("\n", ";", rlang::quo_text(object@methods[[n]]@fc)), max_c),
                          m2 + 1, "right", " ")
            cat("    method: ", p1, "; func: ", p2, "\n", sep="")
        }
        if (length(object@methods) > 5 ) {
            cat("    ... and", length(object@methods) - 5, "more methods.\n")
        }
    }
}

#' Show BenchDesign object
#' 
#' @param object BenchDesign object to show
#' 
#' @importFrom stringr str_pad str_trunc
#' @importFrom rlang quo_text
#' @rdname BenchDesign-show
setMethod("show", signature(object = "BenchDesign"), .show.BenchDesign)

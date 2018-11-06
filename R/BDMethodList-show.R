.show.BDMethodList <- function(object) {
    cat(stringr::str_pad("BenchDesign Method List (BDMethodList) ", 60, "right", "-"), "\n")
    cat("list of", length(object), ifelse(length(object) == 1L, "method", "methods"), "\n")
    if (length(object) < 1) {
        cat("  none\n")
    } else {
        max_c <- 20
        m1 <- max(nchar(names(head(object))))
        m1 <- min(m1, max_c)
        m2 <- max(nchar(sapply(head(object), function(x) { rlang::quo_text(x@fc) })))
        m2 <- min(m2, max_c)
        for (n in names(object)) {
            p1 <- stringr::str_pad(stringr::str_trunc(n, max_c), m1 + 1, "left", " ")
            p2 <- stringr::str_pad(stringr::str_trunc(gsub("\n", ";", rlang::quo_text(object[[n]]@fc)), max_c),
                                   m2 + 1, "right", " ")
            cat("  method: ", p1, "; func: ", p2, "\n", sep="")
        }
    }
}

#' Show BDMethodList object
#' 
#' @param object BDMethodList object to show
#' 
#' @importFrom stringr str_pad str_trunc
#' @importFrom rlang quo_text
#' @rdname BDMethodList-show
setMethod("show", signature(object = "BDMethodList"), .show.BDMethodList)

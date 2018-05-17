.show.BDMethod <- function(object) {
    cat(stringr::str_pad("BenchDesign Method ", 60, "right", pad = "-"), "\n")
    cat("method: ")
    func_q <- rlang::quo_text(object@fc, nlines = 3)
    if (!grepl("^function\\(", func_q)) {
        cat(stringr::str_trunc(func_q, 52))
        func_q <- rlang::quo_text(object@f, nlines = 3)
    }
    func_q <- gsub("\\s*\n\\s*", " ", func_q)
    cat("\n ", stringr::str_trunc(func_q, 58), "\n")
    cat("post:\n")
    if (!is.null(object@post)) {
        post_n <- names(object@post)
        post_q <- lapply(object@post, rlang::quo_text, nlines = 3)
        post_q <- gsub("\\s*\n\\s*", " ", post_q)
        for (i in seq(post_n))
            cat(stringr::str_trunc(paste(" ", post_n[i], ":", post_q[i]), 60), "\n")
    } else {
        cat("  none\n")
    }
    cat("meta:\n")
    if (!is.null(object@meta)) {
        meta <- lapply(object@meta, rlang::eval_tidy)
        meta_n <- names(meta)
        meta_q <- sapply(meta, quo_text)
        for (i in seq(meta_n))
            cat(stringr::str_trunc(paste(" ", meta_n[i], ":", meta_q[i]), 60), "\n")
    } else {
        cat("  none\n")
    }
    
    cat("parameters:\n")
    if (length(object@params) > 0) {
        param_n <- names(object@params)
        param_q <- sapply(object@params, quo_text)
        for (i in seq(param_n))
            cat(stringr::str_trunc(paste(" ", param_n[i], ":", param_q[i]), 60), "\n")
    } else {
        cat("  none\n")
    }
}

#' @importFrom stringr str_pad str_trunc
#' @importFrom rlang quo_text
#' @rdname BDMethod-class
setMethod("show", signature(object = "BDMethod"), .show.BDMethod)


.show.BenchDesign <- function(object) {
    cat(stringr::str_pad("BenchDesign ", 60, "right", "-"), "\n")
    cat("  benchmark data:\n")
    if (!is.null(object@data)) {
        dtype <- object@data@type
        cat("    type:", object@data@type, "\n")
        if (dtype == "data")
            cat("    names:", paste(names(object@data@data), collapse=", "), "\n")
        if (dtype == "md5hash")
            cat(" MD5 hash:", stringr::str_trunc(object@data@data, 50), "\n")
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

#' @importFrom stringr str_pad str_trunc
#' @importFrom rlang quo_text
#' @rdname BenchDesign-class
setMethod("show", signature(object = "BenchDesign"), .show.BenchDesign)

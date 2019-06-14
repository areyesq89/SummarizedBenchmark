.show.BDMethod <- function(object) {
    cat(stringr::str_pad("BenchDesign Method (BDMethod) ", 60, "right", pad = "-"), "\n")
    cat("  method: ")
    func_q <- rlang::quo_text(object@fc, nlines = 3)
    if (!grepl("^function\\(", func_q)) {
        cat(stringr::str_trunc(func_q, 50))
        func_q <- rlang::quo_text(object@f, nlines = 3)
    }
    func_q <- gsub("\\s*\n\\s*", " ", func_q)
    cat("\n   ", stringr::str_trunc(func_q, 56), "\n")
    cat("  parameters:\n")
    if (length(object@params) > 0) {
        param_n <- names(object@params)
        param_q <- sapply(object@params, quo_text)
        for (i in seq(param_n))
            cat(stringr::str_trunc(paste("   ", param_n[i], ":", param_q[i]), 60), "\n")
    } else {
        cat("  none\n")
    }
    cat("  post:\n")
    if (!is.null(object@post)) {
        post_n <- names(object@post)
        post_q <- lapply(object@post, rlang::quo_text, nlines = 3)
        post_q <- gsub("\\s*\n\\s*", " ", post_q)
        for (i in seq(post_n))
            cat(stringr::str_trunc(paste("   ", post_n[i], ":", post_q[i]), 60), "\n")
    } else {
        cat("    none\n")
    }
    cat("  meta:\n")
    if (!is.null(object@meta)) {
        meta_n <- names(object@meta)
        meta_q <- sapply(object@meta, quo_text)
        for (i in seq(meta_n))
            cat(stringr::str_trunc(paste("   ", meta_n[i], ":", meta_q[i]), 60), "\n")
    } else {
        cat("    none\n")
    }
}

#' Show BDMethod object
#' 
#' @param object BDMethod object to show
#' 
#' @importFrom stringr str_pad str_trunc
#' @importFrom rlang quo_text 
#' @rdname BDMethod-show
setMethod("show", signature(object = "BDMethod"), .show.BDMethod)

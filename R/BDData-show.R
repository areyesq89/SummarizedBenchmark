.show.BDData <- function(object) {
    cat(stringr::str_pad("BenchDesign Data (BDData) ", 60, "right", pad = "-"), "\n")
    dtype <- object@type
    cat("  type:", object@type, "\n")
    if (dtype == "data") {
        if (is(object@data, "data.frame")) {
            cat("  data:\n")
            desc <- as.data.frame(object@data)
        } else {
            cat("  data summary:\n")
            desc <- data.frame(name = seq_len(length(object@data)),
                               class = unlist(sapply(object@data, class)),
                               length = unlist(sapply(object@data, length)))
            desc$name <- names(object@data)
            row.names(desc) <- NULL
        }
        print(utils::head(desc, n = 10))
        if (nrow(desc) > 10) 
            cat(" ... with", nrow(desc) - 10, "more rows.\n")
     }   
    if (dtype == "md5hash")
        cat("  MD5 hash:", stringr::str_trunc(object@data, 50), "\n")
}

#' Show BDData object
#' 
#' @param object BDData object to show
#'
#' @return 
#' Print description of BDData object to console
#' 
#' @importFrom stringr str_pad
#' @importFrom utils head
#' @rdname BDData-show
setMethod("show", signature(object = "BDData"), .show.BDData)

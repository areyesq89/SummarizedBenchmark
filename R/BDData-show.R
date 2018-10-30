.show.BDData <- function(object) {
    cat(stringr::str_pad("BenchDesign Data (BDData) ", 60, "right", pad = "-"), "\n")
    dtype <- object@type
    cat("  type:", object@type, "\n")
    if (dtype == "data") {
        if (is(object@data, "data.frame")) {
            cat("  data:\n")
            if (is(object@data, "tibble")) {
                print(object@data)
            } else {
                print(head(object@data, n = 10))
                if (nrow(object@data) > 20) 
                    cat(" ... with", nrow(object@data) - 20, "more rows.\n")
            }
        } else {
            cat("  data summary:\n")
            desc <- dplyr::tibble(names = unlist(sapply(object@data, names)),
                                  class = unlist(sapply(object@data, class)),
                                  length = unlist(sapply(object@data, length)))
            print(desc)
        }
    }
    if (dtype == "md5hash")
        cat("  MD5 hash:", stringr::str_trunc(object@data, 50), "\n")
}

#' Show BDData object
#' 
#' @param object BDData object to show
#' 
#' @importFrom stringr str_pad
#' @rdname BDData-show
setMethod("show", signature(object = "BDData"), .show.BDData)

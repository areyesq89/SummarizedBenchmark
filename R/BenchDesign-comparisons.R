#' Compare BDMethod objects
#'
#' Simple comparison of two BDMethod objects based on
#' meta data.
#' 
#' @param x a \code{BDMethod} object
#' @param y a \code{BDMethod} object
#'
#' @return
#' logical value indicating whether the two objects produced the same
#' meta data.
#'
#' @export
#' @importFrom dplyr all_equal
#' @author Patrick Kimes
compareBDMethods <- function(x, y) {
    if(!is(y, "BDMethod") || !is(y, "BDMethod"))
        stop("Must specify two BDMethod objects to compare.")
    dplyr::all_equal(tidyBDMethod(x), tidyBDMethod(y))
}


#' Compare BDData objects
#'
#' Simple comparison of two BDData objects based on
#' comparing both type and data hash.
#' 
#' @param x a \code{BDData} object
#' @param y a \code{BDData} object
#' 
#' @return
#' list of two values giving agreement of "data" and "type".
#' 
## check if data is the same
compareBDData <- function(x, y) {
    if (!is(x, "BDData") || !is(y, "BDData"))
        stop("Must specify two BDData objects to compare.")
    sameType <- ifelse(x@type == y@type, x@type, FALSE)
    x <- BDDataHash(x)
    y <- BDDataHash(y)
    sameData <- x@data == y@data
    list(data = sameData, type = sameType)
}


#' Compare BenchDesign objects
#'
#' Comparison of BenchDesign objects and BenchDesign method information
#' stored in SummarizedBenchmark objects. Inputs can be either BenchDesign
#' or SummarizedBenchmark objects. If SummarizedBenchmark objects are
#' specified, the method metadata stored in the \code{colData} will be
#' used for the comparison. If only a single SummarizedBenchmark object is
#' specified, the \code{colData} information will be compared with the
#' BenchDesign object in the \code{BenchDesign} slot of the object.
#' To compare the \code{BenchDesign} slots of SummarizedBenchmark objects,
#' the BenchDesigns should be extracted with \code{BenchDesign(sb)} and
#' passed as inputs (see Examples).
#' 
#' @param x a SummarizedBenchmark or BenchDesign object
#' @param y an optional second SummarizedBenchmark or BenchDesign object
#'        (default = NULL)
#' @param ...
#' 
#' @return
#' list of comparison results
#'
#' @examples
#' data(sb)
#'
#' compareBenchDesigns(sb)
#'
#' ## same as above
#' compareBenchDesigns(sb, BenchDesign(sb))
#'
#' @rdname compareBenchDesigns
#' @importFrom rlang enquo
#' @export
#' @author Patrick Kimes
setGeneric("compareBenchDesigns",
           function(x, y = NULL, ...) standardGeneric("compareBenchDesigns"))

.compare.sb <- function(x, y, ...) {
    .compare.sb.bd(x, x@BenchDesign)
}

.compare.sb.sb <- function(x, y, ...) {
    x <- colData(x)[, elementMetadata(colData(x))$colType == "methodInformation"]
    y <- colData(y)[, elementMetadata(colData(y))$colType == "methodInformation"]
    .compare.meta(x, y)
}

.compare.sb.bd <- function(x, y, ...) {
    x <- colData(x)[, elementMetadata(colData(x))$colType == "methodInformation"]
    y <- tidyBDMethod(y)
    .compare.meta(x, y)
}

.compare.bd.sb <- function(x, y, ...) {
    .compare.sb.bd(y, x, ...)
}

.compare.bd.bd <- function(x, y, ...) {
    xm <- tidyBDMethod(x)
    ym <- tidyBDMethod(y)
    .compare.meta(xm, ym)
    .compare.data(x@data, y@data)
}

.compare.meta <- function(x, y, ...) {
    TRUE
}

#' @rdname compareBenchDesigns
setMethod("compareBenchDesigns", signature(x = "SummarizedBenchmark", y = "missing"), .compare.sb)
setMethod("compareBenchDesigns", signature(x = "SummarizedBenchmark", y = "SummarizedBenchmark"), .compare.sb.sb)
setMethod("compareBenchDesigns", signature(x = "SummarizedBenchmark", y = "BenchDesign"), .compare.sb.bd)
setMethod("compareBenchDesigns", signature(x = "BenchDesign", y = "SummarizedBenchmark"), .compare.sb.bd)
setMethod("compareBenchDesigns", signature(x = "BenchDesign", y = "BenchDesign"), .compare.bd.bd)


#' Print Comparison of BenchDesign objects
#'
#' Simple comparison of two BenchDesign objects based on
#' methods meta data and data MD5 hashs.
#'
#' @param x a \code{BenchDesign} object
#' @param y a \code{BenchDesign} object
#'
#' @return
#' logical value indicating whether the two objects have
#' methods producing the same meta data and data with the
#' same MD5 hashes.
#' 
#' @export
#' @author Patrick Kimes
printCompareBenchDesigns <- function(x, y) {
    if (!is(x, "BenchDesign") || !is(y, "BenchDesign"))
        stop("Must specify two BenchDesign objects to compare.")

    if (!is.null(x@data) && !is.null(y@data)) {
        bdd <- compareBDData(x@data, y@data)
    } else {
        bdd <- list(data = NA, type = ifelse(is.null(x@data) && is.null(y@data), "NULL", FALSE))
    }

    cat(stringr::str_pad("BenchDesign Comparison ", 60, "right", "-"), "\n")
    cat("  benchmark data:\n")
    cat("    type: ")
    if (isFALSE(bdd$type)) 
        cat(crayon::red("unequal"),
            "(x:", ifelse(is.null(x@data), "NULL", ifelse(x@data@type == "data", "data,", "MD5 hash,")),
            "y:", ifelse(is.null(y@data), "NULL", ifelse(y@data@type == "data", "data)", "MD5 hash)")), "\n")
    else
        cat(crayon::green("equal"), paste0("(", ifelse(bdd$type == "data", "data", "MD5 hash"), ")"), "\n")
    cat("    MD5 hash: ")
    if (is.na(bdd$data))
        cat(crayon::yellow("NA\n"))
    else
        cat(ifelse(bdd$data, crayon::green("equal"), crayon::red("unequal")), "\n")

    xmn <- names(x@methods)
    ymn <- names(y@methods)
    xmo <- setdiff(xmn, ymn)
    ymo <- setdiff(ymn, xmn)
    xym <- intersect(xmn, ymn)
    
    cat("  benchmark methods:\n")
    if (length(xym) > 0) {
        xymc <- lapply(xym, function(mn) compareBDMethods(x@methods[[mn]], y@methods[[mn]]))
        xymc <- unlist(lapply(xymc, isTRUE))
    }

    cat(crayon::green("      equal: "))
    if (length(xym) > 0 && sum(xymc) > 0)
        cat(paste(xym[xymc], collapse = ", "), "\n")
    else
        cat("-\n")

    cat(crayon::red("    unequal: "))
    if (length(xym) > 0 && sum(!xymc) > 0)
        cat(paste(xym[!xymc], collapse = ", "), "\n")
    else
        cat("-\n")

    cat(crayon::yellow("     x only: "))
    if (length(xmo) > 0)
        cat(paste(xmo, collapse = ", "), "\n")
    else
        cat("-\n")
    
    cat(crayon::yellow("     y only: "))
    if (length(ymo) > 0)
        cat(paste(ymo, collapse = ", "), "\n")
    else
        cat("-\n")
}



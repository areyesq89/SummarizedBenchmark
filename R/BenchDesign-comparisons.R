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
compareBDMethod <- function(x, y) {
    if(!is(y, "BDMethod") || !is(y, "BDMethod"))
        stop("Must specify two BDMethod objects to compare.")
    res_meta <- dplyr::all_equal(tidyBDMethod(x), tidyBDMethod(y))
    res_func <- isTRUE(all.equal(x@f, y@f))
    res_post <- isTRUE(all.equal(x@post, y@post))
    
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
    x <- HashBDData(x)
    y <- HashBDData(y)
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
#' @param functions a logical whether to check if both main and post
#'        functions are also identical using \code{all.equal}. (default = TRUE
#'        except when \code{x} is a SummarizedBenchmark object and \code{y} is
#'        NULL)
#' @param ... other parameters
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

.compare.sb <- function(x, y, functions = FALSE, ...) {
    .compare.sb.bd(x, x@BenchDesign, functions = functions)
}

.compare.sb.sb <- function(x, y, functions = TRUE, ...) {
    x <- .sb2tidymeta(x)
##    x <- dplyr::mutate(x, f = BenchDesign(x)
    y <- .sb2tidymeta(x)
    .compare.meta(x, y)
}

.compare.sb.bd <- function(x, y, functions = TRUE, ...) {
    x <- .sb2tidymeta(x)
    y <- tidyBDMethod(y, label = TRUE)
    .compare.meta(x, y)
}

.compare.bd.sb <- function(x, y, functions = TRUE, ...) {
    x <- tidyBDMethod(x, label = TRUE)
    y <- .sb2tidymeta(x)
    .compare.meta(x, y)
}

.compare.bd.bd <- function(x, y, functions = TRUE, ...) {
    xm <- tidyBDMethod(x, label = TRUE)
    ym <- tidyBDMethod(y, label = TRUE)
    res_met <- .compare.meta(xm, ym)
    res_dat <- compareBDData(x@data, y@data)
    return(list(methods = res_met, data = res_data))
    
    ##func.identical <- isTRUE(all.equal())
    ##post.identical <- ## sort by names, and just run mapply(all.equal)
}

## helper to compare functions (both main and post)
.compare.funs <- function(x, y, ...) {
    isTRUE(is.equal(x@f, y@f))
}

## helper to compare data
.compare.data <- function(x, y, ...) {
}

## helper to compare method meta data
.compare.meta <- function(x, y, ...) {
    stopifnot(tibble::is_tibble(x), tibble::is_tibble(y))
    
    xy <- dplyr::bind_rows(x = x, y = y, .id = "comp.id")
    return(xy)
    ## -- need to be able to combine things later....
    ## func.*  [package version info]
    ## param.* [parameters used]
    ## meta.*  [misc. metadata elements]
    ## function (just direct comparison?)
    ## posts (just direct comparison?)
    ##
    ## ## rough design of output
    ## res_met <- tibble(label = methods,
    ##                   object = c("both", "x", "y"),
    ##                   equal = c(TRUE, FALSE, NA),
    ##                   func = c(TRUE, FALSE, NA),
    ##                   versions = c(TRUE, FALSE, NA),
    ##                   params = c(TRUE, FALSE, NA),
    ##                   meta = c(TRUE, FALSE, NA))
    
    return(list(methods = res_met, data = NULL))
}

## helper function to extract metadata from SummarizedBenchmark coldata
.sb2tidymeta <- function(x) {
    xdf <- colData(x)[, elementMetadata(colData(x))$colType == "methodInformation", drop = FALSE]
    xdf <- dplyr::as_tibble(as.data.frame(xdf, optional = TRUE))
    dplyr::mutate(xdf, label = colnames(x))
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



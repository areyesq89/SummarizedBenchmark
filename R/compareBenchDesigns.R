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
#' @seealso \code{\link{compareBenchDesigns}}
#' @export
#' @importFrom tibble is_tibble
#' @import dplyr
#' @importFrom tidyr gather spread
#' @author Patrick Kimes
compareBDMethod <- function(x, y) {
    if(!is(y, "BDMethod") || !is(y, "BDMethod"))
        stop("Must specify two BDMethod objects to compare.")
    xt <- dplyr::as_tibble(tidyBDMethod(x))
    yt <- dplyr::as_tibble(tidyBDMethod(y))

    ## compare main and post functions
    res_fn <- all.equal(x@f, y@f)
    res_fp <- all.equal(x@post, y@post)

    ## check function info
    res_v <- dplyr::all_equal(dplyr::select(xt, starts_with("func.")), dplyr::select(yt, starts_with("func.")))
    res_v <- isTRUE(res_v)
    
    ## check parameter info
    res_p <- dplyr::all_equal(dplyr::select(xt, starts_with("param.")), dplyr::select(yt, starts_with("param.")))
    res_p <- isTRUE(res_p)
    
    ## check metadata info
    res_m <- dplyr::all_equal(dplyr::select(xt, starts_with("meta.")), dplyr::select(yt, starts_with("meta.")))
    res_m <- isTRUE(res_m)
    
    list(f = isTRUE(res_fn), version = isTRUE(res_v), params = isTRUE(res_p),
         meta = isTRUE(res_m), post = isTRUE(res_fp))
}


#' Compare BDData objects
#'
#' Simple comparison of two BDData objects based on
#' comparing both type and data hash.
#' 
#' @param x a \code{BDData} or \code{BenchDesign} object
#' @param y a \code{BDData} or \code{BenchDesign} object
#' 
#' @return
#' list of two values giving agreement of "data" and "type".
#' 
#' @seealso \code{\link{compareBenchDesigns}}
#' @export
#' @author Patrick Kimes
compareBDData <- function(x, y) {
    if (is(x, "BenchDesign"))
        x <- BDData(x)
    if (is(y, "BenchDesign"))
        y <- BDData(y)
    if (is.null(x) && is.null(y))
        return(list(data = NULL, type = TRUE))
    if (is.null(x) || is.null(y))
        return(list(data = NULL, type = FALSE))
    if (!is(x, "BDData") || !is(y, "BDData"))
        stop("Must specify two BDData or NULL objects to compare.")
    sameType <- ifelse(x@type == y@type, x@type, FALSE)
    x <- hashBDData(x)
    y <- hashBDData(y)
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
#' @param ... other parameters
#' 
#' @return
#' list of comparison results
#'
#' @seealso \code{\link{compareBDMethod}}, \code{\link{compareBDData}}
#' @name compareBenchDesigns
#' @author Patrick Kimes
NULL

.compare.sb <- function(x, y, ...) {
    .compare.base(x = x, y = BenchDesign(x))
}

.compare.base <- function(x, y, ...) {
    xt <- .tidyForComparison(x) 
    yt <- .tidyForComparison(y)
    
    res_met <- .compare.meta(xt, yt)
    res_dat <- compareBDData(BDData(x), BDData(y))

    return(list(methods = res_met, data = res_dat))
}


## helper to compare method meta data
## NOTE: not using compareBDMethod since SummarizedBenchmarks need special handling.
##       lapply BDMethod is simpler, but currently not meant for checking SB metadata columns.
##       .compare.meta also returns nice tabular output.
##       columns should be consistent w/ compareBDMethod
.compare.meta <- function(x, y, ...) {
    stopifnot(tibble::is_tibble(x), tibble::is_tibble(y))

    ## determine unique - careful of case when BD is empty
    x_label <- if (nrow(x)) x$label else c()
    y_label <- if (nrow(y)) y$label else c()
    mxo <- setdiff(x_label, y_label)
    myo <- setdiff(y_label, x_label)
    mxy <- intersect(x_label, y_label)

    xj <- dplyr::filter(x, label %in% mxy)
    yj <- dplyr::filter(y, label %in% mxy)

    ## check function info
    xyv <- lapply(mxy, function(l) {
        isTRUE(dplyr::all_equal(dplyr::select(dplyr::filter(xj, label == l), starts_with("func.")),
                                dplyr::select(dplyr::filter(yj, label == l), starts_with("func."))))
    })
    names(xyv) <- mxy

    ## check parameter info
    xyp <- lapply(mxy, function(l) {
        isTRUE(dplyr::all_equal(dplyr::select(dplyr::filter(xj, label == l), starts_with("param.")),
                                dplyr::select(dplyr::filter(yj, label == l), starts_with("param."))))
    })
    names(xyp) <- mxy
    
    ## check metadata info - drop any NA columns (diffs can arise from other methods)
    xym <- lapply(mxy, function(l) {
        xna <- dplyr::select(dplyr::filter(xj, label == l), starts_with("meta."))
        yna <- dplyr::select(dplyr::filter(yj, label == l), starts_with("meta."))
        isTRUE(dplyr::all_equal(dplyr::select_if(xna, funs(!any(is.na(.)))),
                                dplyr::select_if(yna, funs(!any(is.na(.))))))
    })
    names(xym) <- mxy

    ## check functions
    xyfn <- lapply(mxy, function(l) {
        isTRUE(all.equal(dplyr::filter(xj, label == l)$f, dplyr::filter(yj, label == l)$f))
    })
    names(xyfn) <- mxy

    ## check post functions
    xyfp <- lapply(mxy, function(l) {
        isTRUE(all.equal(dplyr::filter(xj, label == l)$post, dplyr::filter(yj, label == l)$post))
    })
    names(xyfp) <- mxy

    ## combine all contrasts
    mxy <- dplyr::bind_rows(list(f = xyfn, version = xyv, params = xyp, meta = xym, post = xyfp),
                            .id = "comparison")
    if (nrow(mxy)) {
        mxy <- tidyr::gather(mxy, label, value, -comparison)
        mxy <- tidyr::spread(mxy, comparison, value)
    } else {
        mxy <- NULL
    }
    
    ## add in rows for xonly, yonly methods
    mxy <- dplyr::bind_rows(Both = mxy,
                            xOnly = if (length(mxo)) { dplyr::tibble(label = mxo) },
                            yOnly = if (length(myo)) { dplyr::tibble(label = myo) },
                            .id = "overlap")
    
    return(list(res = mxy, x, y))
}

## helper function to extract metadata from SummarizedBenchmark coldata
.tidyForComparison <- function(x) {
    if (is(x, "SummarizedBenchmark")) {
        infocols <- elementMetadata(colData(x))$colType == "methodInformation"
        infocols <- unlist(lapply(infocols, isTRUE))
        xt <- colData(x)[, infocols, drop = FALSE]
        xt <- dplyr::as_tibble(as.data.frame(xt, optional = TRUE))
        xt <- dplyr::mutate(xt, label = colnames(x))
    } else if (is(x, "BenchDesign")) {
        ## clean up empty @post slots in BenchDesign for comparison
        x <- makePostLists(x)
        xt <- tidyBDMethod(x, label = TRUE)
    }

    if (length(BDMethodList(x)) > 0) {
        xf <- lapply(BDMethodList(x), slot, "f")
        xf <- dplyr::tibble(label = names(xf), f = xf)
        xp <- lapply(BDMethodList(x), slot, "post")
        xp <- dplyr::tibble(label = names(xp), post = xp)
        
        xt <- dplyr::left_join(xt, xf, by = "label")
        xt <- dplyr::left_join(xt, xp, by = "label")
    }
    
    xt
}

#' @rdname compareBenchDesigns
#' @export
setMethod("compareBenchDesigns", signature(x = "SummarizedBenchmark", y = "missing"), .compare.sb)

#' @rdname compareBenchDesigns
#' @export
setMethod("compareBenchDesigns", signature(x = "SummarizedBenchmark", y = "SummarizedBenchmark"), .compare.base)

#' @rdname compareBenchDesigns
#' @export
setMethod("compareBenchDesigns", signature(x = "SummarizedBenchmark", y = "BenchDesign"), .compare.base)

#' @rdname compareBenchDesigns
#' @export
setMethod("compareBenchDesigns", signature(x = "BenchDesign", y = "SummarizedBenchmark"), .compare.base)

#' @rdname compareBenchDesigns
#' @export
setMethod("compareBenchDesigns", signature(x = "BenchDesign", y = "BenchDesign"), .compare.base)

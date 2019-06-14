#' Check/Update SummarizedBenchmark
#'
#' @description
#' Function to update or check status of \code{\link[=SummarizedBenchmark-class]{SummarizedBenchmark}}
#' results.
#'
#' If only a \code{\link[=SummarizedBenchmark-class]{SummarizedBenchmark}} object is specified,
#' the function will check whether `func`, `param`, `meta`, `post` or the `pkg_vers` of the
#' methods in the \code{\link[=BenchDesign-class]{BenchDesign}} stored with the
#' \code{\link[=SummarizedBenchmark-class]{SummarizedBenchmark}} do not match values stored in the
#' \code{colData}. By default, no methods will be executed to update results. To actually execute updates,
#' set \code{dryrun = FALSE}.
#'
#' If a \code{\link[=BenchDesign-class]{BenchDesign}} object is specified in addition to a
#' \code{\link[=SummarizedBenchmark-class]{SummarizedBenchmark}} object, the function will check which
#' methods in the new \code{\link[=BenchDesign-class]{BenchDesign}} need to be executed to update the 
#' \code{\link[=SummarizedBenchmark-class]{SummarizedBenchmark}} results. Again, by default, no methods
#' will be executed unless \code{dryrun = FLASE} is specified.
#'
#' Unless \code{reuseParams = FALSE} is specified, the parameters of the last execution session stored
#' in the the \code{colData} of the \code{\link[=SummarizedBenchmark-class]{SummarizedBenchmark}} object
#' will be used.
#' 
#' @param sb a \code{\link[=SummarizedBenchmark-class]{SummarizedBenchmark}} object
#' @param bd a \code{\link[=BenchDesign-class]{BenchDesign}} object
#' @param dryrun logical whether to just print description of what would
#'        be updated rather than actually running any methods. (default = TRUE)
#' @param version logical whether to re-run methods with only package
#'        version differences. (default = FALSE)
#' @param keepAll logical whether to keep methods run in original \code{\link[=SummarizedBenchmark-class]{SummarizedBenchmark}}
#'        but not in new \code{\link[=BenchDesign-class]{BenchDesign}}. Only used if \code{bd} is not NULL. (default = TRUE)
#' @param reuseParams logical whether to reuse parameters from \code{buildBench} call
#'        used to create \code{\link[=SummarizedBenchmark-class]{SummarizedBenchmark}} object (if available). Directly
#'        specified \code{\link{buildBench}} parameters still take precedence. (default = TRUE)
#' @param ... optional parameters to pass to \code{\link{buildBench}}.
#'
#' @return
#' SumamrizedBenchmark object.
#'
#' @examples
#' ## load example SummarizedBenchmark object
#' data(allSB)
#' sb <- allSB[[1]]
#'
#' ## check if results are out of date
#' updateBench(sb)
#'
#' ## modify BenchDesign
#' bd <- BenchDesign(sb)
#' bd <- dropMethod(bd, "kallisto-default")
#' 
#' ## check if results need to be updated with new BenchDesign
#' updateBench(sb, bd)
#' 
#' @seealso \code{\link{buildBench}}
#' @import dplyr
#' @importFrom SummarizedExperiment cbind
#' @importFrom crayon red yellow green bold
#' @importFrom stringr str_pad str_trunc
#' @importFrom S4Vectors elementMetadata
#' @export
#' @author Patrick Kimes
updateBench <- function(sb, bd = NULL, dryrun = TRUE, version = FALSE, keepAll = TRUE,
                        reuseParams = TRUE, ...) {
    ## will only run if original SummarizedBenchmark contains BenchDesign data slot
    if (is.null(BDData(sb)))
        stop("Data used to generate original SummarizedBenchmark is unavailable.\n",
             "The data must be available to ensure that the same data is being used to ",
             "evaluate any new or updated methods.")

    if (is.null(bd))
        bd <- BenchDesign(sb)
    
    ## capture buildBench parameters
    bbp <- list(...)
    if ("data" %in% names(bbp)) {
        bd@data <- BDData(bbp$data)
        bbp$data <- NULL
    }

    ## use original data if not specified
    if (is.null(BDData(bd)))
        bd@data <- BDData(sb)

    ## get buildBench call parameters from last session
    old_sessions <- metadata(sb)$sessions
    n_sessions <- length(old_sessions)
    if (reuseParams && n_sessions) {
        oldbbp <- old_sessions[[n_sessions]]$parameters
        bbp <- replace(oldbbp, names(bbp), bbp)
    }

    ## just print verbose description of actions if specified
    if (dryrun) {
        res <- .printUpdateBench(sb, bd, version = version, keepAll = keepAll)
        return(invisible(res))
    }

    ## compare methods
    res <- compareBenchDesigns(sb, bd)

    ## need to run 'yOnly' methods
    newrun_set <- dplyr::filter(res$methods$res, overlap == "yOnly")$label

    ## need to run updated/changed methods (or if data is new)
    rerun_set <- dplyr::filter(res$methods$res, overlap == "Both")
    if (!isTRUE(res$data$data)) {
        rerun_set <- rerun_set$label
    } else if (version) {
        rerun_set <- dplyr::filter(rerun_set, !f | !meta | !params | !post | !version)$label
    } else {
        rerun_set <- dplyr::filter(rerun_set, !f | !meta | !params | !post)$label
    }
    
    ## construct method list and pick data set
    bdm <- BDMethodList(bd)[c(newrun_set, rerun_set)]
    bdd <- .select.bddata(sb, bd)
    bdnew <- BenchDesign(methods = bdm, data = bdd)

    ## run buildBench only if needed
    if (length(bdm)) {
        sbnew <- tryCatch(
            do.call(buildBench, c(list(bd = bdnew), bbp)),
            error = function(e) {
                message("!! error caught while updating methods in updateBench w/ buildBench !!\n",
                        "!! returning original SummarizedBenchmark object        !!\n",
                        "!! original message: \n", e)
                return(NULL)
            })
        if (!is.null(sbnew)) {
            if (!keepAll)
                sb <- sb[, intersect(names(BDMethodList(sb)),
                                     names(BDMethodList(bd)))]
            sb <- .combineSummarizedBenchmarks(sb, sbnew)
        }
    }
    
    ## return results
    return(sb)
}



## helper to decide whether to use the BDData from the SB object or BD object
## requires determining if data MD5 hashes match, and whether either BDData
## contains complete data (vs. a MD5 hash value).
.select.bddata <- function(sb, bd) {
    if (BDData(bd)@type == "data") {
        return(BDData(bd))
    } else if (isTRUE(compareBDData(BDData(sb), BDData(bd))$data)) {
        if (BDData(sb)@type == "data")
            return(BDData(sb))
        else
            stop("Need to provide non-hash data set.\n",
                 "Specified objects only contain MD5 hash data sets.\n",
                 "MD5 hash = ", BDData(sb)@data, ".\n",
                 "MD5 hash values can be checked using digest::digest.")
    } else {
        stop("Need to provide non-NULL and non-hash data set.\n",
             "Specified objects only contain MD5 hash or NULL data sets.\n",
             "SummarizedBenchmark MD5 hash =", BDData(sb)@data, ".\n",
             "BenchDesign MD5 hash =", BDData(bd)@data, ".\n",
             "MD5 hash values can be checked using digest::digest.")
    }
}



## helper to handle merging of SummarizedBenchmark objects
##
## NOTE: This is not meant to be for general use. Here, sb2 is assumed to
##       be a newer SummarizedBenchmark object created from running
##       updateBench with sb1 and a newer BenchDesign object. Therefore,
##       if the same method is run in both, the results from
##
## @author Patrick Kimes
.combineSummarizedBenchmarks <- function(sb1, sb2) {
    stopifnot(is(sb1, "SummarizedBenchmark"), is(sb2, "SummarizedBenchmark"))
    
    ## combine BenchDesign objects
    bd1 <- BDMethodList(sb1)
    bd2 <- BDMethodList(sb2)
    keep_set <- setdiff(colnames(sb1), colnames(sb2))
    bd1 <- bd1[keep_set]
    bdd <- BDData(sb2)
    bd <- BenchDesign(methods = c(bd1, bd2), data = bdd)

    ## subset original results on methods not rerun
    sb1 <- sb1[, names(bd1)]

    ## combine column data as expanded parameter sets in each object before merging
    emd1 <- dplyr::as_tibble(as.data.frame(elementMetadata(colData(sb1)), optional = TRUE))
    emd1 <- dplyr::mutate(emd1, colid = colnames(colData(sb1)))
    emd2 <- dplyr::as_tibble(as.data.frame(elementMetadata(colData(sb2)), optional = TRUE))
    emd2 <- dplyr::mutate(emd2, colid = colnames(colData(sb2)))
    emd <- dplyr::bind_rows(emd1, emd2)
    emd <- dplyr::distinct(emd)
    if (any(duplicated(emd$colid)))
        stop("Duplicated column IDs with inconsistent elementMetaData.")
    emd_id <- emd$colid
    emd <- DataFrame(dplyr::select(emd, -colid))

    coldat1 <- colData(sb1)
    coldat1 <- as.data.frame(coldat1, optional = TRUE)
    coldat1 <- dplyr::as_tibble(coldat1, rownames = "label")
    coldat2 <- colData(sb2)
    coldat2 <- as.data.frame(coldat2, optional = TRUE)
    coldat2 <- dplyr::as_tibble(coldat2, rownames = "label")
    coldat2$session.idx <- length(metadata(sb1)$sessions) + 1 ## increment session index for newer SB
    coldat <- dplyr::bind_rows(coldat1, coldat2)
    coldat <- DataFrame(dplyr::select(coldat, -label), row.names = coldat$label)
    colData(sb1) <- coldat[colnames(sb1), , drop = FALSE]
    colData(sb2) <- coldat[colnames(sb2), , drop = FALSE]

    elementMetadata(colData(sb1)) <- emd[match(colnames(colData(sb1)), emd_id), , drop = FALSE]
    elementMetadata(colData(sb2)) <- emd[match(colnames(colData(sb2)), emd_id), , drop = FALSE]

    ## combine assay sets by filling in missing assays with NA assays
    aNames1 <- assayNames(sb1)
    aNames2 <- assayNames(sb2)
    aNames1o <- setdiff(aNames1, aNames2)
    aNames2o <- setdiff(aNames2, aNames1)
    for (i in aNames1o)
        assay(sb2, i) <- matrix(NA, nrow = nrow(sb2), ncol = ncol(sb2))
    for (i in aNames2o)
        assay(sb1, i) <- matrix(NA, nrow = nrow(sb1), ncol = ncol(sb1))
    
    ## if row dimensions don't match, just return as list of the original 2 inputs
    
    ## combine session informations into sessions of 1 object before merging
    sess1 <- metadata(sb1)$sessions
    sess1 <- lapply(sess1, function(x) { x$methods <- intersect(x$methods, keep_set); x })
    sess1 <- lapply(sess1, function(x) { x$results <- x$results[x$methods]; x })
    sess2 <- metadata(sb2)$sessions
    sess <- c(sess1, sess2)
    metadata(sb1)$sessions <- sess
    metadata(sb2)$sessions <- NULL

    ## merge and replace BenchDesign slot
    sb <- SummarizedExperiment::cbind(sb1, sb2)
    sb@BenchDesign <- bd
    sb
}

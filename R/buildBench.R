#' Make SummarizedBenchmark from BenchDesign
#'
#' Function to evaluate \code{BenchDesign} methods on supplied
#' data set to generate a \code{SummarizedBenchmark}. In addition
#' to the results of applying each method on the data, the returned
#' \code{SummarizedBenchmark} also includes metadata for the methods
#' in the \code{colData} of the returned object, metadata for the
#' data in the \code{rowData}, and the session information generated
#' by \code{sessionInfo()} in the \code{metadata}. 
#' 
#' @param bd \code{BenchDesign} object.
#' @param data Data set to be used for benchmarking, will take priority over
#'        data set originally specified to BenchDesign object. 
#'        Ignored if NULL. (default = NULL)
#' @param truthCols Character vector of column names in data set corresponding to
#'        ground truth values for each assay. If specified, column will be added to
#'        the \code{groundTruth} DataFrame for the returned SummarizedBenchmark object.
#'        If the \code{BenchDesign} includes only a single assay, the same name
#'        will be used for the assay. If the \code{BenchDesign} includes multiple assays,
#'        to map data set columns with assays, the vector must have names corresponding
#'        to the assay names specified to the \code{post} parameter at each
#'        \code{addMethod} call. (default = NULL)
#' @param ftCols Vector of character names of columns in data set that should be
#'        included as feature data (row data) in the returned SummarizedBenchmark
#'        object. (default = NULL)
#' @param catchErrors logical whether errors produced by methods during evaluation
#'        should be caught and printed as a message without stopping the entire
#'        build process. (default = TRUE)
#' @param sortIDs Whether the output of each method should be merged using IDs.
#'        If TRUE, each method must return a named vector or list. The names will be
#'        used to align the output of each method in the returned SummarizedBenchmark.
#'        Missing values will be set to NA. This can be useful if the different methods
#'        return overlapping, but not identical, results. If \code{truthCols} is also
#'        specified, and sorting by IDs is necessary, rather than specifying 'TRUE',
#'        specify the string name of a column in the \code{data} to use to sort the
#'        method output to match the order of \code{truthCols}. (default = FALSE)
#' @param parallel Whether to use parallelization for evaluating each method.
#'        Parallel execution is performed using \code{BiocParallel}. Parameters for
#'        parallelization should be specified with \code{BiocParallel::register} or
#'        through the \code{BPPARAM} parameter. (default = FALSE)
#' @param BPPARAM Optional \code{BiocParallelParam} instance to be used when
#'        \code{parallel} is TRUE. If not specified, the default instance from the
#'        parameter registry is used.
#'
#' @details
#' Parallelization is performed across methods. Therefore, there is no benefit to
#' specifying more cores than the total number of methods in the \code{BenchDesign}
#' object.
#' By default, errors thrown by individual methods in the \code{BenchDesign} are caught
#' during evaluation and handled in a way that allows \code{buildBench} to continue
#' running with the other methods. The error is printed as a message, and the corresponding
#' column in the returned \code{SummarizedBenchmark} object is set to NA. Since
#' many benchmarking experiments can be time and computationally intensive, having to rerun
#' the entire analysis due to a single failed method can be frustrating. Default error catching
#' was included to alleviate these frustrations. However, if this behavior is not desired,
#' setting \code{catchErrors = FALSE} will turn off error handling.
#' 
#' @return
#' \code{SummarizedBenchmark} object with single assay
#'
#' @examples
#' ## with toy data.frame
#' df <- data.frame(pval = rnorm(100))
#' bench <- BenchDesign(df)
#'
#' ## add methods
#' bench <- addMethod(bench, label = "bonf", func = p.adjust,
#'                    params = rlang::quos(p = pval, method = "bonferroni"))
#' bench <- addMethod(bench, label = "BH", func = p.adjust,
#'                    params = rlang::quos(p = pval, method = "BH"))
#'
#' ## evaluate benchmark experiment
#' sb <- buildBench(bench)
#'
#' ## evaluate benchmark experiment w/ data sepecified
#' sb <- buildBench(bench, data = df)
#' 
#' @import BiocParallel
#' @importFrom dplyr bind_rows
#' @importFrom utils packageName packageVersion sessionInfo
#' @export
#' @author Patrick Kimes
buildBench <- function(bd, data = NULL, truthCols = NULL, ftCols = NULL, sortIDs = FALSE,
                       catchErrors = TRUE, parallel = FALSE, BPPARAM = bpparam()) {

    if (!is.null(data) && !is.list(data)) {
        stop("If specified, 'data' must be a list or data.frame object.")
    }
    if (!is.null(data)) {
        bd@data <- new("BDData", data = data, type = "data")
    }
    
    ## make sure data is provided
    if (is.null(bd@data)) {
        stop("data in BenchDesign is NULL.\n",
             "Please specify a non-NULL dataset to build SummarizedBenchmark.")
    }
    
    if (bd@data@type != "data") {
        stop("data in BenchDesign is only hash.\n",
             "Please specify a non-NULL dataset to build SummarizedBenchmark.")
    }

    ## make sure methods are specified
    if (length(bd@methods) == 0) {
        stop("list of methods in BenchDesign is empty.\n",
             "Please specify at least one method to build SummarizedBenchmark.")
    }
    

    ## CLEAN UP METHODS POSTPROCESSING FUNCTIONS ---------------------------------------
    
    ## fill post-processing methods with null method if not specified
    bd@methods <- lapply(bd@methods, function(x) { if (length(x@post) < 1) { x@post <- list(default = function(z) { z }) }; x })

    ## determine number of assay to generate
    assay_names <- lapply(bd@methods, function(x) names(x@post))
    uassays <- unique(unlist(assay_names))
    nassays <- length(uassays)
    
    ## fill in missing functions
    bd@methods <- lapply(bd@methods, function(x) { x@post <- x@post[uassays]; x })
    
    fillpost <- function(mp) {
        lapply(mp, function(y) { ifelse(is.null(y), function(z) { NA }, y) })
    }
    bd@methods <- lapply(bd@methods, function(x) { x@post <- fillpost(x@post); x })
    
    ## ---------------------------------------------------------------------------------
    
    
    ## check validity of sortIDs value (unit logical or data column name)
    stopifnot(length(sortIDs) == 1)
    stopifnot(is.logical(sortIDs) || is.character(sortIDs))
    if (is.character(sortIDs)) {
        if (sortIDs %in% names(bd@data@data)) {
            sortID_col <- sortIDs
            sortIDs <- TRUE
        } else {
            stop("Specified 'sortIDs' column is not in the specified data set.")
        }
    } else {
        sortID_col <- NULL
    }

    ## check if truthCols is in bdata and 1-dim vector
    if (!is.null(truthCols)) {
        if (length(truthCols) == 1 && is.null(names(truthCols)))
            names(truthCols) <- "default"

        stopifnot(truthCols %in% names(bd@data@data))
        stopifnot(length(truthCols) <= nassays)
        stopifnot(names(truthCols) %in% uassays)
        stopifnot(!is.null(names(truthCols)))
        
        if (sortIDs && is.null(sortID_col))
            stop("If 'truthCols' is specified, 'sortIDs' can not simply be 'TRUE'.\n",
                 "Instead, specify a column in the data to use for sorting the output to ",
                 "match the order of the 'truthCols'.")
    }
    
    ## check if ftCols are in bdata
    if (!is.null(ftCols) && !all(ftCols %in% names(bd@data@data))) {
        stop("Invalid ftCols specification. ",
             "ftCols must be a subset of the column names of the input data.")
    }
        
    ## check validity of parallel values (unit logical value) 
    stopifnot((length(parallel) == 1) && is.logical(parallel))
    
    ## assay: evaluate all functions
    if (parallel) {
        a <- evalMethodsParallel(bd, catchErrors, BPPARAM)
    } else {
        a <- evalMethods(bd, catchErrors)
    }
    
    ## handle multi-assay separately
    if (sortIDs) {
        if (any(unlist(lapply(a, function(x) { lapply(x, function(y) { is.null(names(y)) }) })))) {
            stop("If sortIDs = TRUE, all methods must return a named list or vector.")
        }
        a <- lapply(uassays, function(x) { lapply(a, `[[`, x) })
        a <- .list2mat(a)
        if (!is.null(sortID_col)) {
            ## reorder by specified ID column
            a <- lapply(a, .expandrows, rid = bd@data@data[[sortID_col]])
        }
    } else {
        ## note difference in lapply vs. sapply
        a <- lapply(uassays, function(x) { sapply(a, `[[`, x) })
    }
    names(a) <- uassays

    
    ## colData: method information
    df <- tidyBDMethods(bd@methods, dat = bd@data@data)
    
    ## performanceMetrics: empty
    pf <- rep(list("bench" = list()), nassays)
    names(pf) <- uassays
    pf <- SimpleList(pf)

    ## metadata: record sessionInfo
    md <- list(sessionInfo = sessionInfo())
    
    ## list of initialization parameters
    sbParams <- list(assays = a,
                     colData = df,
                     performanceMetrics = pf,
                     metadata = md,
                     BenchDesign = bd)
    
    ## pull out grouthTruth if available
    if (!is.null(truthCols)) {
        sbParams[["groundTruth"]] <- DataFrame(bd@data@data[truthCols])
        
        if (length(a) == 1 && names(a) == "default") {
            ## rename assay to match groundTruth
            names(sbParams[["assays"]]) <- truthCols
            names(sbParams[["performanceMetrics"]]) <- truthCols
        } else {
            ## rename grouthTruth to match assays for named assays case
            names(sbParams[["groundTruth"]]) <- names(truthCols)
        }
    }

    ## add feature columns if specified
    if (!is.null(ftCols)) {
        sbParams[["ftData"]] <- DataFrame(bd@data@data[ftCols])
    }
    
    do.call(SummarizedBenchmark, sbParams)
}



## helper function to take list of "method = value-vector" pairs,
## match on value names and return as single data.frame
.list2mat <- function(x) {
    lapply(x, function(z) {
        z <- dplyr::tibble(.method = names(z),
                           .id = lapply(z, names),
                           .val = z)
        z <- tidyr::unnest(z)
        z <- tidyr::spread(z, .method, .val)
        z <- data.frame(dplyr::select(z, -.id),
                        row.names = z$.id)
        as(z, "matrix")
    })
}


## helper function to take list of matrices and expand rows to
## include only rows in specified 'rid' (row ID set)
.expandrows <- function(x, rid) {
    xnew <- matrix(nrow = length(rid), ncol = ncol(x),
                   dimnames = list(rid, colnames(x)))
    ovnames <- intersect(rid, rownames(x))
    xnew[ovnames, ] <- x[ovnames, ]
    xnew
}


## helper to evaluate a single BDMethod
evalMethod <- function(bdm, lab, dat, ce) {
    expr <- rlang::quo((bdm@f)(!!! bdm@params))
    expr <- rlang::quo({ z <- (!! expr); lapply(bdm@post, function(zz) { zz(z) }) })
    tryCatch(
        rlang::eval_tidy(expr, dat),
        error = function(e) {
            message("!! error caught in buildBench !!\n",
                    "!! error in method: ", lab)
            if (ce) {
                message("!!  original message: \n",
                        "!!  ", e)
                return(NA)
            } else {
                stop(e)
            }
        })
}


## helper function to evaluate all quosures with data
evalMethods <- function(bd, ce) {
    mapply(evalMethod, bdm = bd@methods, lab = names(bd@methods),
           MoreArgs = list(dat = bd@data@data, ce = ce), SIMPLIFY = FALSE)
}


## helper function to evaluate using BiocParallel
evalMethodsParallel <- function(bd, ce, BPPARAM) {
    bpmapply(evalMethod, bdm = bd@methods, lab = names(bd@methods),
             MoreArgs = list(dat = bd@data@data, ce = ce), SIMPLIFY = FALSE,
             BPPARAM = BPPARAM)
}



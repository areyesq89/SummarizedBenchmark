#' Execute BenchDesign
#'
#' Function to evaluate methods defined in a \code{\link[=BenchDesign-class]{BenchDesign}} on a supplied
#' data set to generate a \code{\link[=SummarizedBenchmark-class]{SummarizedBenchmark}} of
#' benchmarking results. In addition to the results of applying each method on the data, the returned
#' \code{\link[=SummarizedBenchmark-class]{SummarizedBenchmark}} also
#' includes metadata for the methods
#' in the \code{colData} of the returned object, metadata for the
#' data in the \code{rowData}, and session information in the
#' \code{metadata}.
#'
#' @param bd \code{\link[=BenchDesign-class]{BenchDesign}} object.
#' @param data Data set to be used for benchmarking, will take priority over
#'        data set specified to \code{\link[=BenchDesign-class]{BenchDesign}} object.
#'        Ignored if \code{NULL}. (default = \code{NULL})
#' @param truthCols Character vector of column names in data set corresponding to
#'        ground truth values for each assay. If specified, column will be added to
#'        the \code{groundTruth} DataFrame of the returned
#'        \code{\link[=SummarizedBenchmark-class]{SummarizedBenchmark}} object.
#'        If the \code{\link[=BenchDesign-class]{BenchDesign}} includes only a single assay, the same name
#'        will be used for the assay. If the \code{\link[=BenchDesign-class]{BenchDesign}}
#'        includes multiple assays,
#'        to map data set columns with assays, the vector must have names corresponding
#'        to the assay names specified to the \code{post} parameter at each
#'        \code{addMethod} call. (default = \code{NULL})
#' @param ftCols Vector of character names of columns in data set that should be
#'        included as feature data (row data) in the returned \code{\link[=SummarizedBenchmark-class]{SummarizedBenchmark}}
#'        object. (default = \code{NULL})
#' @param catchErrors logical whether errors produced by methods during evaluation
#'        should be caught and printed as a message without stopping the entire
#'        build process. (default = \code{TRUE})
#' @param keepData Whether to store the data as part of the \code{\link[=BenchDesign-class]{BenchDesign}} slot of the
#'        returned \code{\link[=SummarizedBenchmark-class]{SummarizedBenchmark}} object. If \code{FALSE}, a MD5 hash of the data will be
#'        stored with the \code{\link[=BenchDesign-class]{BenchDesign}} slot. (default = \code{FALSE})
#' @param sortIDs Whether the output of each method should be merged and sorted using IDs.
#'        See Details for more information. (default = \code{FALSE})
#' @param parallel Whether to use parallelization for evaluating each method.
#'        Parallel execution is performed using \pkg{BiocParallel}. Parameters for
#'        parallelization should be specified with \code{\link[BiocParallel]{register}} or
#'        through the \code{BPPARAM} parameter. (default = \code{FALSE})
#' @param BPPARAM Optional \code{BiocParallelParam} instance to be used when
#'        \code{parallel} is \code{TRUE}. If not specified, the default instance from the
#'        parameter registry is used.
#'
#' @details
#' Parallelization is performed across methods. Therefore, there is currently no benefit to
#' specifying more cores than the total number of methods in the
#' \code{\link[=BenchDesign-class]{BenchDesign}}
#' object.
#'
#' By default, errors thrown by individual methods in the \code{\link[=BenchDesign-class]{BenchDesign}} are caught
#' during evaluation and handled in a way that allows \code{\link{buildBench}} to continue
#' running with the other methods. The error is printed as a message, and the corresponding
#' column in the returned \code{\link[=SummarizedBenchmark-class]{SummarizedBenchmark}}
#' object is set to \code{NA}. Since
#' many benchmarking experiments can be time and computationally intensive, having to rerun
#' the entire analysis due to a single failed method can be frustrating. Default error catching
#' was included to alleviate these frustrations. However, if this behavior is not desired,
#' setting \code{catchErrors = FALSE} will turn off error handling.
#'
#' If \code{sortIDs = TRUE}, each method must return a named vector or list. The names will be
#' used to align the output of each method in the returned \code{\link[=SummarizedBenchmark-class]{SummarizedBenchmark}}.
#' Missing values
#' from each method will be set to NA. This can be useful if the different methods return
#' overlapping, but not identical, results. If \code{truthCols} is also specified, and sorting
#' by IDs is necessary, rather than specifying \code{sortIDs = TRUE}, specify the string name of a column in
#' the \code{data} to use to sort the method output to match the order of \code{truthCols}.
#'
#' When a method specified in the \code{\link[=BenchDesign-class]{BenchDesign}} does not have a postprocessing function specified
#' to \code{post =}, the trivial \code{base::identity} function is used as the default postprocessing
#' function.
#'
#' @return
#' \code{\link[=SummarizedBenchmark-class]{SummarizedBenchmark}} object.
#'
#' @examples
#' ## with toy data.frame
#' df <- data.frame(pval = rnorm(100))
#' bench <- BenchDesign(data = df)
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
#' @seealso \code{\link{updateBench}}
#' @import BiocParallel
#' @importFrom sessioninfo session_info
#' @import dplyr
#' @importFrom tidyr spread unnest
#' @importFrom rlang quo eval_tidy !! !!!
#' @importFrom utils packageName packageVersion
#' @export
#' @author Patrick Kimes
buildBench <- function(bd, data = NULL, truthCols = NULL, ftCols = NULL, sortIDs = FALSE,
                       keepData = FALSE, catchErrors = TRUE, parallel = FALSE, BPPARAM = bpparam()) {

    ## ## capture args for future analysis
    arglist <- as.list(environment())
    arglist <- arglist[names(arglist) != "bd"]
    arglist <- arglist[names(arglist) != "data"]

    if (!is.null(data) && !is.list(data)) {
        stop("\nIf specified, 'data' must be a list or data.frame object.\n")
    }
    if (!is.null(data)) {
        bd@data <- new("BDData", data = data, type = "data")
    }
    
    ## make sure data is provided
    if (is.null(bd@data)) {
        stop("\ndata in BenchDesign is NULL.\n",
             "Please specify a non-NULL dataset to build SummarizedBenchmark.\n")
    }

    if (bd@data@type != "data") {
        stop("\ndata in BenchDesign is only a MD5 hash.\n",
             "Please specify a non-NULL dataset to build SummarizedBenchmark.\n")
    }

    ## make sure methods are specified
    if (length(bd@methods) == 0) {
        stop("\nlist of methods in BenchDesign is empty.\n",
             "Please specify at least one method to build SummarizedBenchmark.\n")
    }

    ## clean up NULL @post slots in BenchDesign
    bd <- makePostLists(bd)

    ## determine final post function names and count
    uassays <- unique(unlist(lapply(bd@methods, function(x) { names(x@post) })))
    nassays <- length(uassays)

    ## check validity of sortIDs value (unit logical or data column name)
    stopifnot(length(sortIDs) == 1)
    stopifnot(is.logical(sortIDs) || is.character(sortIDs))
    if (is.character(sortIDs)) {
        if (sortIDs %in% names(bd@data@data)) {
            sortID_col <- sortIDs
            sortIDs <- TRUE
        } else {
            stop("\nInvalid 'sortIDs' specification.\n",
                 "Please specify only valid column names for 'sortIDs'.\n")
        }
    } else {
        sortID_col <- NULL
    }

    ## check if truthCols is in data and 1-dim vector
    if (!is.null(truthCols)) {
        if (is.null(names(truthCols)) && length(truthCols) == 1L && nassays == 1L) {
            names(truthCols) <- uassays
        } else if (is.null(names(truthCols)) && length(truthCols) == 1L && nassays > 1L) {
            warning("\tDropping 'truthCols' since it cannot be matched with an assay.\n")
        }

        if (!all(names(truthCols) %in% uassays))
            warning("\nDropping 'truthCols' not matching assay names.\n")

        truthCols <- truthCols[names(truthCols) %in% uassays]
        if (!length(truthCols))
            truthCols <- NULL
        
        if (!is.null(truthCols) && !all(truthCols %in% names(bd@data@data)))
            stop("\nInvalid 'truthCols' specification.\n",
                 "Please specify only valid column names as 'truthCols'.\n")
        
        if (!is.null(truthCols) && sortIDs && is.null(sortID_col))
            stop("\nIf 'truthCols' is specified, 'sortIDs' must specify a column\n",
                 "in the data to use for sorting the output to match the order of\n",
                 "the 'truthCols'.")
    }

    ## check if ftCols are in data
    if (!is.null(ftCols) && !all(ftCols %in% names(bd@data@data))) {
        stop("\nInvalid ftCols specification.\n",
             "Please only specify valid column names as 'ftCols'.\n")
    }

    ## check validity of parallel values (unit logical value)
    stopifnot((length(parallel) == 1) && is.logical(parallel))

    ## assay: evaluate all functions
    if (parallel) {
        a <- evalMethodsParallel(bd, catchErrors, BPPARAM)
    } else {
        a <- evalMethods(bd, catchErrors)
    }

    ## determine how each method faired
    a_res <- lapply(a, function(x) { x[unlist(lapply(x, is, "buildbench-error"))] })
    a_res <- lapply(names(a), function(i) {
        a_resi <- a_res[[i]]
        a_resi[setdiff(uassays, names(a[[i]]))] <- "missing"
        a_resi[setdiff(uassays, names(a_resi))] <- "success"
        a_resi
    })
    names(a_res) <- names(a)
        
    ## reshape results / method:post -> post:method
    a <- lapply(uassays, function(x) { lapply(a, `[[`, x) })
    a <- lapply(a, function(x)
        x[!unlist(lapply(x, function(zz) { is(zz, "buildbench-error") || is.null(zz) }))])

    ## identify any assays with no methods returning results
    aNAi <- unlist(lapply(a, length)) == 0L
    a <- a[!aNAi]

    ## determine output row order
    if (!is.null(sortID_col)) {
        siVals <- bd@data@data[[sortID_col]]
    } else if (length(a) > 1) {
        siVals <- lapply(a, lapply, names)
        if (any(unlist(lapply(siVals, lapply, is.null))))
            siVals <- NULL
        else
            siVals <- unique(unlist(siVals))
    } else {
        siVals <- NULL
    }
    
    ## some assays might have only NA results
    if (length(a) == 0L) {
        stop("\nNo method returned valid values for any assays!\n")
    }
    
    ## fill in missing methods with NAs, return in fixed order
    a <- lapply(a, eval2assay, si = sortIDs, siv = siVals)
    a <- lapply(a, function(x) {
        ms <- setdiff(names(bd@methods), colnames(x))
        msl <- list()
        msl[ms] <- NA
        x <- do.call(cbind, c(list(x), msl))
        x[, names(bd@methods), drop = FALSE]
    })
    names(a) <- uassays[!aNAi]

    if (any(aNAi)) {
        nr <- nrow(a[[1]])
        aNA <- matrix(nrow = nr, ncol = length(names(bd@methods)),
                      dimnames = list(NULL, names(bd@methods)))
        aNA <- rep(list(aNA), sum(aNAi))
        names(aNA) <- uassays[aNAi]
        a <- c(a, aNA)
    }
    
    ## colData: method information
    df <- tidyBDMethod(bd, dat = bd@data@data, eval = TRUE)
    
    ## add session info to df
    df$session.idx <- 1
    ##df$session.result <- a_res[names(bd@methods)]
    
    ## performanceMetrics: empty
    pf <- rep(list("bench" = list()), nassays)
    names(pf) <- uassays
    pf <- SimpleList(pf)

    ## metadata: record sessionInfo
    md <- list(sessions = list(list(methods = names(bd@methods),
                                    results = a_res,
                                    parameters = arglist,
                                    sessionInfo = sessioninfo::session_info())))

    ## list of initialization parameters
    sbParams <- list(assays = a,
                     colData = df,
                     performanceMetrics = pf,
                     metadata = md,
                     BenchDesign = bd)

    ## pull out grouthTruth if available
    if (!is.null(truthCols)) {
        sbParams[["groundTruth"]] <- DataFrame(bd@data@data[truthCols])
        names(sbParams[["groundTruth"]]) <- truthCols

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
        names(sbParams[["ftData"]]) <- ftCols
    }

    ## BenchDesign: replace data with MD5 hash
    if (!keepData) {
        sbParams[["BenchDesign"]]@data <- hashBDData(sbParams[["BenchDesign"]]@data)
    }

    do.call(SummarizedBenchmark, sbParams)
}



## helper function to take list of "method = value-vector" pairs,
## match on value names and return as single data.frame
.list2mat <- function(z) {
    z <- dplyr::tibble(.method = names(z),
                       .id = lapply(z, names),
                       .val = z)
    z <- tidyr::unnest(z)
    z <- tidyr::spread(z, .method, .val)
    z <- data.frame(dplyr::select(z, -.id),
                    row.names = z$.id, check.names=FALSE)
    as(z, "matrix")
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
    ## define main function as an expression
    expr <- rlang::quo((bdm@f)(!!! bdm@params))
    ## first evaluate main function
    z <- tryCatch(rlang::eval_tidy(expr, dat),
                  error = function(e) {
                      message("!! error caught in buildBench !!\n",
                              "!! error in main function of method: '", lab, "'")
                      if (ce) {
                          message("!!  original message: \n",
                                  "!!  ", conditionMessage(e))
                          return(structure(conditionMessage(e), class = "buildbench-error",
                                           origin = "main"))
                      } else {
                          stop(e)
                      }})
    ## return list of same error of length equal to post functions
    if (is(z, "buildbench-error")) {
        zres <- rep(list(z), length(bdm@post))
    } else {
        ## else run with post functions
        zres <- lapply(names(bdm@post), function(zzi) {
            tryCatch(bdm@post[[zzi]](z),
                     error = function(e) {
                         message("!! error caught in buildBench !!\n",
                                 "!! error in method: '", lab, "', post: '", zzi, "'")
                         if (ce) {
                             message("!!  original message: \n",
                                     "!!  ", conditionMessage(e))
                             return(structure(conditionMessage(e), class = "buildbench-error",
                                              origin = "post"))
                         } else {
                             stop(e)
                         }})
        })
    }
    names(zres) <- names(bdm@post)
    zres
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



## helper function to make all post slots in a BenchDesign non-NULL list
makePostLists <- function(db) {
    uassays <- unique(unlist(lapply(db@methods, function(x) names(x@post))))

    base_fnl <- list(default = base::identity)
    if (length(uassays) == 1) {
        names(base_fnl) <- uassays
    }

    dbml <- lapply(db@methods, function(x) { if (length(x@post) < 1) { x@post <- base_fnl }; x })
    db@methods <- BDMethodList(dbml)
    return(db)
}



eval2assay <- function(al, si, siv) {
    if (si && any(is.null(lapply(al, names))))
        stop("\nIf sortIDs = TRUE, all methods must return a named list or vector.\n")
    if (si) {
        alr <- .list2mat(al)
        if (!is.null(siv)) {
            alr <- .expandrows(alr, rid = siv)
        }
    } else {
        alr <- simplify2array(al, higher = FALSE)
        if (!is(alr, "matrix")) {
            if (!all(unlist(lapply(al, is, "list"))))
                warning("\nMethod outputs could not be reduced to matrix.\n")
            alr <- rbind(al)
        }
        rownames(alr) <- NULL
    }
    return(alr)
}

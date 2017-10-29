#' Make SummarizedBenchmark from BenchDesign
#'
#' Function to evaluate \code{BenchDesign} methods on supplied
#' data set to generate a \code{SummarizedBenchmark}.
#' 
#' @param b \code{BenchDesign} object.
#' @param data Data set to be used for benchmarking, will take priority over
#'        data set originally specified to BenchDesign object. 
#'        Ignored if NULL. (default = NULL)
#' @param truthCol Character name of column in data set corresponding to ground
#'        truth values. If specified, column will be added to \code{groundTruth}
#'        DataFrame for the returned SummarizedBenchmark object, and same name
#'        will be used for assay. (default = NULL)
#' @param ftCols Vector of character names of columns in data set that should be
#'        included as feature data (row data) in the returned SummarizedBenchmark
#'        object. (default = NULL)
#' @param ptabular Whether to return method parameters with each parameter in a
#'        separate column of the \code{colData} for the returned SummarizedBenchmark
#'        object, i.e. in tabular form. If FALSE, method parameters are returned
#'        as a single column with comma-delimited "key=value" pairs. (default = TRUE)
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
#' 
#' @return
#' \code{SummarizedBenchmark} object with single assay
#'
#' @import BiocParallel
#' @importFrom data.table rbindlist
#' @importFrom utils packageName packageVersion
#' @export
#' @author Patrick Kimes
buildBench <- function(b, data = NULL, truthCol = NULL, ftCols = NULL,
                       ptabular = TRUE, parallel = FALSE, BPPARAM = bpparam()) {

    if (!is.null(data)) {
        b$bdata <- data
    }
    
    ## make sure data is actually specified
    if (is.null(b$bdata)) {
        stop("data in BenchDesign is NULL.\n",
             "Please specify a non-NULL dataset to build SummarizedBenchmark.")
    }

    ## check if truthCol is in bdata and 1-dim vector
    if (!is.null(truthCol)) {
        stopifnot(truthCol %in% names(b$bdata))
        stopifnot(dim(b$bdata$truthCol) == 1)
    }

    ## check if ftCols are in bdata
    if (!is.null(ftCols)) {
        stopifnot(ftCols %in% names(b$bdata))
    }

    stopifnot((length(ptabular) == 1) && is.logical(ptabular))
    stopifnot((length(parallel) == 1) && is.logical(parallel))
    
    ## assay: evaluate all functions
    if (parallel) {
        a <- evalBMethodsParallel(b, BPPARAM)
    } else {
        a <- evalBMethods(b)
    }
    a <- do.call(cbind, a)
    a <- list("bench" = a)

    ## colData: method information
    df <- cleanBMethods(b, ptabular)
    
    ## performanceMetrics: empty
    pf <- SimpleList(list("bench" = list()))


    ## list of initialization parameters
    sbParams <- list(assays = a,
                     colData = df,
                     performanceMetrics = pf)

    ## rename assay to match groundTruth column is specified
    if (!is.null(truthCol)) {
        names(sbParams[["assays"]]) <- truthCol
        names(sbParams[["performanceMetrics"]]) <- truthCol

        sbParams[["groundTruth"]] <- DataFrame(b$bdata[truthCol])
    }

    ## add feature columns if specified
    if (!is.null(ftCols)) {
        sbParams[["ftData"]] <- DataFrame(b$bdata[ftCols])
    }

    do.call(SummarizedBenchmark, sbParams)
}


## helper function to evaluate all quosures with data
evalBMethods <- function(b) {
    al <- lapply(seq(b$methods),
                 function(i) {
                     x <- b$methods[[i]]
                     expr <- quo(UQ(x$func)(!!! x$dparams))
                     if (is.function(eval_tidy(x$post, b$bdata))) {
                         expr <- quo(UQ(x$post)(!! expr))
                     }
                     tryCatch(
                         eval_tidy(expr, b$bdata),
                         error = function(e) {
                             message("!! error caught in buildBench !!\n",
                                     "!! error in method: ", names(b$methods)[i], "\n",
                                     "!!  original message: \n",
                                     "!!  ", e)
                             return(NA)
                         })
                 })
    names(al) <- names(b$methods)
    al
}


## helper function to evaluate using BiocParallel
evalBMethodsParallel <- function(b, BPPARAM) {
    al <- bplapply(seq(b$methods),
                   function(i) {
                       x <- b$methods[[i]]
                       expr <- quo(UQ(x$func)(!!! x$dparams))
                       if (is.function(eval_tidy(x$post, b$bdata))) {
                           expr <- quo(UQ(x$post)(!! expr))
                       }
                       tryCatch(
                           eval_tidy(expr, b$bdata),
                           error = function(e) {
                               message("!! error caught in buildBench !!\n",
                                       "!! error in method: ", names(b$methods)[i], "\n",
                                       "!!  original message: \n",
                                       "!!  ", e)
                               return(NA)
                           })
                   },
                   BPPARAM = BPPARAM)
    names(al) <- names(b$methods)
    al
}


## helper function to convert method info to character for colData
cleanBMethods <- function(b, ptabular) {
    df <- lapply(b$methods, cleanBMethod, bdata = b$bdata, ptabular = ptabular)
    df <- data.table::rbindlist(df, fill=TRUE)
    df$blabel <- names(b$methods)
    df
}

cleanBMethod <- function(m, bdata, ptabular) {
    ## parse package/version information
    bmeta <- funcMeta(eval_tidy(m$func))

    ## parse primary method
    if (bmeta$is_anon[1]) {
        bfunc <- gsub("\n", ";", quo_text(m$func))
    } else {
        bfunc <- quo_name(m$func)
    }

    ## parse method parameters
    if (ptabular) {
        bparams <- sapply(m$dparams, quo_text)
        names(bparams) <- paste0("param.", names(bparams))
        bparams <- data.frame(t(bparams))
    } else {
        bparams <- paste(names(m$dparams), "=",
                         sapply(m$dparams, quo_text),
                         collapse=", ")
        bparams <- data.frame(bparams)
    }
    
    ## parse postprocessing method
    has_post <- is.function(eval_tidy(m$post, bdata))
    if (has_post) {
        bpost <- gsub("\n", ";", quo_text(m$post))
    } else {
        bpost <- NA_character_
    }

    cbind(data.frame(bfunc, bpost), bparams, bmeta)
}


## helper to gather important information for function
funcMeta <- function(f) {
    fenv <- environment(f)

    pkg_name <- packageName(fenv)
    pkg_anon <- is.null(pkg_name)

    if (pkg_anon) {
        pkg_name <- NA_character_
        pkg_vers <- NA_character_
    } else {
        pkg_vers <- as(packageVersion(pkg_name), "character")
    }
    
    data.frame(is_anon = pkg_anon, pkg_name = pkg_name, pkg_vers = pkg_vers)
}




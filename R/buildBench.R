#' Make SummarizedBenchmark from BenchDesign
#'
#' Function to evaluate BenchDesign methods on supplied
#' data set to generate a SummarizedBenchmark.
#' 
#' @param b BenchDesign object
#' @param data data set to be used in place of `bdata` for benchmarking;
#'        ignored if NULL (default = NULL)
#' @param truthCol character name of column in data set corresponding to ground
#'        truth values. If specified, column will be added to `groundTruth`
#'        DataFrame of returned SummarizedBenchmark object, and same name
#'        will be used for assay (default = NULL)
#' @param ptabular whether to return parameters in tabular form (defualt = TRUE)
#' 
#' @return
#' SummarizedBenchmark with one assay
#'
#' @importFrom data.table rbindlist
#' @export
#' @author Patrick Kimes
buildBench <- function(b, data = NULL, truthCol = NULL, ptabular = TRUE) {

    if (!is.null(data)) {
        b$bdata <- data
    }
    
    ## make sure data is actually specified
    if (is.null(b$bdata)) {
        stop("data in BenchDesign is NULL.\nPlease specify a non-NULL dataset to build SummarizedBenchmark.")
    }

    ## check if truthCol is in bdata and 1-dim vector
    if (!is.null(truthCol)) {
        stopifnot(truthCol %in% names(b$bdata))
        stopifnot(dim(b$bdata$truthCol) == 1)
    }                  
    
    ## assay: evaluate all functions
    a <- eval_defaults(b)
    a <- do.call(cbind, a)
    a <- list("bench" = a)
    
    ## colData: method information
    df <- clean_methods(b, ptabular)
    
    ## performanceMetrics: empty
    pf <- SimpleList(list("bench" = list()))

    ## groundTruth
    if (!is.null(truthCol)) {
        gt <- DataFrame(V1 = b$bdata[[truthCol]])
        names(a) <- truthCol
        names(pf) <- truthCol
        names(gt) <- truthCol
    
        SummarizedBenchmark(assays = a,
                            colData = df,
                            performanceMetrics = pf,
                            groundTruth = gt)
    } else {
        SummarizedBenchmark(assays = a,
                            colData = df,
                            performanceMetrics = pf)
    }
}


## helper function to evaluate all quosures with data
eval_defaults <- function(b) {
        lapply(b$methods,
               function(x) {
                   expr <- quo(UQ(x$func)(!!! x$dparams))
                   if (is.function(eval_tidy(x$post, b$bdata))) {
                       expr <- quo(UQ(x$post)(!! expr))
                   }
                   eval_tidy(expr, b$bdata)
               })
}


## helper function to convert method info to character for colData
clean_methods <- function(b, ptabular) {
    df <- lapply(b$methods, clean_method, bdata = b$bdata, ptabular = ptabular)
    df <- data.table::rbindlist(df, fill=TRUE)
    df$blabel <- names(b$methods)
    df
}

clean_method <- function(m, bdata, ptabular) {
    ## parse package/version information
    bmeta <- func_meta(eval_tidy(m$func))

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
func_meta <- function(f) {
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




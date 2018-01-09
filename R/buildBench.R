#' Make SummarizedBenchmark from BenchDesign
#'
#' Function to evaluate \code{BenchDesign} methods on supplied
#' data set to generate a \code{SummarizedBenchmark}.
#' 
#' @param b \code{BenchDesign} object.
#' @param data Data set to be used for benchmarking, will take priority over
#'        data set originally specified to BenchDesign object. 
#'        Ignored if NULL. (default = NULL)
#' @param truthCols Character vector of column names in data set corresponding to
#'        ground truth values for each assay. If specified, column will be added to
#'        the \code{groundTruth} DataFrame for the returned SummarizedBenchmark object.
#'        If the \code{BenchDesign} includes only a single assay, the same name
#'        will be used for the assay. If the \code{BenchDesign} includes multiple assays,
#'        to map data set columns with assays, the vector must have names corresponding
#'        to the assay names specified to the \code{bpost} parameter at each
#'        \code{addBMethod} call. (default = NULL)
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
buildBench <- function(b, data = NULL, truthCols = NULL, ftCols = NULL,
                       ptabular = TRUE, parallel = FALSE, BPPARAM = bpparam()) {

    if (!is.null(data)) {
        b$bdata <- data
    }
    
    ## make sure data is provided
    if (is.null(b$bdata)) {
        stop("data in BenchDesign is NULL.\n",
             "Please specify a non-NULL dataset to build SummarizedBenchmark.")
    }

    ## make sure methods are specified
    if (length(b$methods) == 0) {
        stop("list of methods in BenchDesign is empty.\n",
             "Please specify at least one method to build SummarizedBenchmark.")
    }
    
    ## determine whether bpost was specified as a list
    assay_aslist <- sapply(b$methods, function(x) { is.list(eval_tidy(x$post, b$bdata)) })
    assay_aslist <- unique(assay_aslist)
    if (length(assay_aslist) > 1) {
        stop("Inconsistent bpost specification style across methods. ",
             "If bpost is specified as a list for any method, it must be specified ",
             "as a list for all methods.")
    }
    
    ## determine number of assay to generate
    nassays <- sapply(b$methods, function(x) { length(eval_tidy(x$post, b$bdata)) })
    nassays <- unique(nassays)
    if (all(nassays %in% 0:1)) {
        nassays <- 1
    } else if (length(nassays) > 1) {
        stop("Inconsistent bpost length across methods. ",
             "If bpost is specified as a list for any method, it must be specified ",
             "as a list of the same length for all methods.")
    }

    ## if multiple assays are used, make sure that all are same name
    if (assay_aslist) {
        assay_names <- lapply(b$methods, function(x) { names(eval_tidy(x$post, b$bdata)) })
        assay_names <- unique(unlist(assay_names))
        if (length(assay_names) != nassays) {
            stop("Inconsistent bpost naming across methods. ",
                 "If bpost is specified as a list for any method, it must be specified ",
                 "as a list of the same length, with the same names, for all methods.")
        }
    } else {
        if (is.null(truthCols)) {
            assay_names <- "bench"
        } else {
            assay_names <- truthCols
        }
    }

    ## check if truthCols is in bdata and 1-dim vector
    if (!is.null(truthCols)) {
        stopifnot(truthCols %in% names(b$bdata),
                  length(truthCols) == nassays)
        if (assay_aslist &&
            (!all(names(truthCols) %in% assay_names) || is.null(names(truthCols)))) {
            stop("Invalid truthCols specification. ",
                 "If bpost is specified as a list and truthCols is also specified, ",
                 "truthCols must be a named list with the same names as bpost.")
        }
    }
    
    ## check if ftCols are in bdata
    if (!is.null(ftCols) && !all(ftCols %in% names(b$bdata))) {
        stop("Invalid ftCols specification. ",
             "ftCols must be a subset of the column names of the input data.")
    }

    ## check validity of ptabular, parallel values (unit logical value) 
    stopifnot((length(ptabular) == 1) && is.logical(ptabular))
    stopifnot((length(parallel) == 1) && is.logical(parallel))
    
    ## assay: evaluate all functions
    if (parallel) {
        a <- evalBMethodsParallel(b, BPPARAM)
    } else {
        a <- evalBMethods(b)
    }

    ## handle mult-assay separately
    if (assay_aslist) {
        a <- lapply(assay_names, function(x) { sapply(a, `[[`, x) })
        names(a) <- assay_names
    } else {
        a <- do.call(cbind, a)
        a <- list("bench" = a)
    }
    
    ## colData: method information
    df <- cleanBMethods(b, ptabular)
    
    ## performanceMetrics: empty
    pf <- rep(list("bench" = list()), nassays)
    names(pf) <- assay_names
    pf <- SimpleList(pf)
    
    ## list of initialization parameters
    sbParams <- list(assays = a,
                     colData = df,
                     performanceMetrics = pf)
    
    ## pull out grouthTruth if available
    if (!is.null(truthCols)) {
        sbParams[["groundTruth"]] <- DataFrame(b$bdata[truthCols])
        if (assay_aslist) {
            ## rename grouthTruth to match assays for named assays case
            names(sbParams[["groundTruth"]]) <- names(truthCols)
        } else {
            ## rename assay to match groundTruth
            names(sbParams[["assays"]]) <- truthCols
            names(sbParams[["performanceMetrics"]]) <- truthCols
        }
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
                     } else if (is.list(eval_tidy(x$post, b$bdata))) {
                         epost <- eval_tidy(x$post, b$bdata)
                         enames <- names(epost)
                         if (all(sapply(epost, is_function))) {
                             expr <- quo({
                                 z <- (!! expr)
                                 lapply(UQ(x$post), function(zz) { zz(z) })
                             })
                         }
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
                       } else if (is.list(eval_tidy(x$post, b$bdata))) {
                           epost <- eval_tidy(x$post, b$bdata)
                           enames <- names(epost)
                           if (all(sapply(epost, is_function))) {
                               expr <- quo({
                                   z <- (!! expr)
                                   lapply(UQ(x$post), function(zz) { zz(z) })
                               })
                           }
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
    df <- mapply(cleanBMethod, m = b$methods, mname = names(b$methods), 
                 MoreArgs = list(bdata = b$bdata, ptabular = ptabular),
                 SIMPLIFY = FALSE)
    df <- data.table::rbindlist(df, fill=TRUE)
    df$blabel <- names(b$methods)
    df
}

cleanBMethod <- function(m, mname, bdata, ptabular) {
    ## check if `meta =` is specified for method
    if (!is.null(m$meta)) {
        if (!is(m$meta, "list") ||
            length(names(m$meta)) != length(m$meta) ||
            is.null(names(m$meta)) ||
            any(names(m$meta) == "")) {
            warning(paste0("bmeta parameter specified for method '",
                           mname, "' will not be used. ",
                           "meta must be a list of named entries."))
            m$meta <- NULL
        }
    }

    ## parse package/version information
    bmeta <- funcMeta(m$func, m$meta)
    
    ## parse primary method
    if (bmeta$bfunc_anon[1]) {
        bfunc <- gsub("\n", ";", quo_text(m$func))
    } else {
        bfunc <- quo_text(m$func)
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
funcMeta <- function(f, meta) {

    ## determine if main `bfunc` was anonymous
    ## -- If an anonymous bfunc was specified directly to *BMethod, then the
    ##    capturing envirnment of the anonymous function will be a child of the
    ##    SummarizedBenchmark environment.
    ##    Handle this by greping for a 
    f_anon <- is.null(packageName(environment(eval_tidy(f)))) ||
        grepl("^\\s*function\\s*\\(", quo_text(f))

    fsrc <- eval_tidy(f)
    vers_src <- "bfunc"
    if ("pkg_func" %in% names(meta)) {
        vers_src <- "bmeta_func"
        fsrc <- eval_tidy(meta$pkg_func)
    } else if ("pkg_name" %in% names(meta) |
               "pkg_name" %in% names(meta)) {
        pkg_name <- ifelse("pkg_name" %in% names(meta),
                           meta$pkg_name, NA_character_)
        pkg_vers <- ifelse("pkg_vers" %in% names(meta),
                           meta$pkg_vers, NA_character_)
        vers_src <- "bmeta_manual"
        fsrc <- NULL
    }

    if (!is.null(fsrc)) {
        if (f_anon) {
            pkg_name <- NA_character_
            pkg_vers <- NA_character_
        } else {
            fenv <- environment(fsrc)
            pkg_name <- packageName(fenv)
            pkg_vers <- as(packageVersion(pkg_name), "character")
        }
    }
    
    res <- data.frame(bfunc_anon = f_anon, vers_src = vers_src,
                      pkg_name = pkg_name, pkg_vers = pkg_vers)

    ## need to parse and merge manually defined metadata columns 
    if (!is.null(meta)) {
        meta_func <- meta[["pkg_func"]] 
        meta <- meta[!(names(meta) %in% c("pkg_func", "pkg_name", "pkg_vers"))]
        if (length(meta) > 0) {
            names(meta) <- paste0("meta.", names(meta))
        }
        if (!is.null(meta_func)) {
            meta_func <- gsub("\n", ";", quo_text(meta_func))
            if (nchar(meta_func) > 20) {
                meta_func <- "omitted (>20 char)"
            }
            meta <- c(meta, "pkg_func" = meta_func)
        }
        res <- c(res, meta)
    }

    res
}




#' Make SummarizedBenchmark from BenchDesign
#'
#' Function to evaluate \code{BenchDesign} methods on supplied
#' data set to generate a \code{SummarizedBenchmark}.
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
#' @param ptabular Whether to return method parameters with each parameter in a
#'        separate column of the \code{colData} for the returned SummarizedBenchmark
#'        object, i.e. in tabular form. If FALSE, method parameters are returned
#'        as a single column with comma-delimited "key=value" pairs. (default = TRUE)
#' @param sortIDs Whether the output of each method should be merged using IDs.
#'        If TRUE, each method must return a named vector or list. The names will be
#'        used to align the output of each method in the returned SummarizedBenchmark.
#'        Missing values will be set to NA. This can be useful if the different methods
#'        return overlapping, but not identical, results. If \code{truthCols} is also
#'        specified, and sorting by IDs is necessary, rather than specifying 'TRUE',
#'        specify the string name of a column in the \code{data} to use to sort the
#'        method output to match the order of  \code{truthCols}. (default = FALSE) 
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
#' @examples
#' ## with toy data.frame
#' df <- data.frame(pval = rnorm(100))
#' bench <- BenchDesign(df)
#'
#' ## add methods
#' bench <- addMethod(bench, label = "bonf", func = p.adjust,
#'                    p = pval, method = "bonferroni")
#' bench <- addMethod(bench, label = "BH", func = p.adjust,
#'                    p = pval, method = "BH")
#'
#' ## evaluate benchmark experiment
#' sb <- buildBench(bench)
#'
#' ## evaluate benchmark experiment w/ data sepecified
#' sb <- buildBench(bench, data = df)
#' 
#' @import BiocParallel
#' @importFrom dplyr bind_rows
#' @importFrom utils packageName packageVersion
#' @export
#' @author Patrick Kimes
buildBench <- function(bd, data = NULL, truthCols = NULL, ftCols = NULL, ptabular = TRUE,
                       sortIDs = FALSE, parallel = FALSE, BPPARAM = bpparam()) {

    if (!is.null(data)) {
        bd$bdata <- data
    }
    
    ## make sure data is provided
    if (is.null(bd$bdata)) {
        stop("data in BenchDesign is NULL.\n",
             "Please specify a non-NULL dataset to build SummarizedBenchmark.")
    }

    ## make sure methods are specified
    if (length(bd$methods) == 0) {
        stop("list of methods in BenchDesign is empty.\n",
             "Please specify at least one method to build SummarizedBenchmark.")
    }
    
    ## determine whether post was specified as a list
    assay_aslist <- sapply(bd$methods, function(x) { is.list(eval_tidy(x$post, bd$bdata)) })
    assay_aslist <- unique(assay_aslist)
    if (length(assay_aslist) > 1) {
        stop("Inconsistent post specification style across methods. ",
             "If post is specified as a list for any method, it must be specified ",
             "as a list for all methods.")
    }
    
    ## determine number of assay to generate
    nassays <- sapply(bd$methods, function(x) { length(eval_tidy(x$post, bd$bdata)) })
    nassays <- unique(nassays)
    if (all(nassays %in% 0:1)) {
        nassays <- 1
    } else if (length(nassays) > 1) {
        stop("Inconsistent post length across methods. ",
             "If post is specified as a list for any method, it must be specified ",
             "as a list of the same length for all methods.")
    }

    ## if multiple assays are used, make sure that all are same name
    if (assay_aslist) {
        assay_names <- lapply(bd$methods, function(x) { names(eval_tidy(x$post, bd$bdata)) })
        assay_names <- unique(unlist(assay_names))
        if (length(assay_names) != nassays) {
            stop("Inconsistent post naming across methods. ",
                 "If post is specified as a list for any method, it must be specified ",
                 "as a list of the same length, with the same names, for all methods.")
        }
    } else {
        if (is.null(truthCols)) {
            assay_names <- "bench"
        } else {
            assay_names <- truthCols
        }
    }
    
    ## check validity of sortIDs value (unit logical or data column name)
    stopifnot(length(sortIDs) == 1)
    stopifnot(is.logical(sortIDs) || is.character(sortIDs))
    if (is.character(sortIDs)) {
        if (sortIDs %in% names(bd$bdata)) {
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
        stopifnot(truthCols %in% names(bd$bdata),
                  length(truthCols) == nassays)
        if (assay_aslist &&
            (!all(names(truthCols) %in% assay_names) || is.null(names(truthCols)))) {
            stop("Invalid 'truthCols' specification. ",
                 "If 'post' is specified as a list and 'truthCols' is also specified, ",
                 "'truthCols' must be a list with names matching 'post'.")
        }
        if (sortIDs && is.null(sortID_col)) {
            stop("If 'truthCols' is specified, 'sortIDs' can not simply be 'TRUE'.\n",
                 "Instead, specify a column in the data to use for sorting the output to ",
                 "match the order of the 'truthCols'.")
        }
    }
    
    ## check if ftCols are in bdata
    if (!is.null(ftCols) && !all(ftCols %in% names(bd$bdata))) {
        stop("Invalid ftCols specification. ",
             "ftCols must be a subset of the column names of the input data.")
    }
        
    ## check validity of ptabular, parallel values (unit logical value) 
    stopifnot((length(ptabular) == 1) && is.logical(ptabular))
    stopifnot((length(parallel) == 1) && is.logical(parallel))
    
    ## assay: evaluate all functions
    if (parallel) {
        a <- evalMethodsParallel(bd, BPPARAM)
    } else {
        a <- evalMethods(bd)
    }

    ## handle multi-assay separately
    if (assay_aslist) {
        if (sortIDs) {
            if (any(unlist(lapply(a, function(x) { lapply(x, function(y) { is.null(names(y)) }) })))) {
                stop("If sortIDs = TRUE, all methods must return a named list or vector.")
            }
            a <- lapply(assay_names, function(x) { lapply(a, `[[`, x) })
            a <- .list2mat(a)
            if (!is.null(sortID_col)) {
                ## reorder by specified ID column
                a <- lapply(a, .expandrows, rid = bd$bdata[[sortID_col]])
            }
        } else {
            ## note difference in lapply vs. sapply
            a <- lapply(assay_names, function(x) { sapply(a, `[[`, x) })
        }
        names(a) <- assay_names
    } else {
        if (sortIDs) {
            if (any(unlist(lapply(a, function(x) { is.null(names(x)) })))) {
                stop("If sortIDs = TRUE, all methods must return a named list or vector.")
            }
            a <- .list2mat(list("bench" = a))
            if (!is.null(sortID_col)) {
                ## reorder by specified ID column
                a$bench <- .expandrows(a$bench, rid = bd$bdata[[sortID_col]])
            }
        } else {
            if (length(unique(sapply(a[!is.na(a)], length))) > 1) {
                stop("Not all methods returned list or vector of same length.\n",
                     "If this is expected, consider setting sortIDs = TRUE and ",
                     "requiring all methods to return named lists or vectors.")
            }
            a <- list("bench" = do.call(cbind, a))
        }
    }
    
    ## colData: method information
    df <- cleanMethods(bd, ptabular)
    
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
        sbParams[["groundTruth"]] <- DataFrame(bd$bdata[truthCols])
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
        sbParams[["ftData"]] <- DataFrame(bd$bdata[ftCols])
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


## helper function to evaluate all quosures with data
evalMethods <- function(bd) {
    al <- lapply(seq(bd$methods),
                 function(i) {
                     x <- bd$methods[[i]]
                     expr <- quo(UQ(x$func)(!!! x$params))
                     if (is.function(eval_tidy(x$post, bd$bdata))) {
                         expr <- quo(UQ(x$post)(!! expr))
                     } else if (is.list(eval_tidy(x$post, bd$bdata))) {
                         epost <- eval_tidy(x$post, bd$bdata)
                         enames <- names(epost)
                         if (all(sapply(epost, is_function))) {
                             expr <- quo({
                                 z <- (!! expr)
                                 lapply(UQ(x$post), function(zz) { zz(z) })
                             })
                         }
                     }
                     tryCatch(
                         eval_tidy(expr, bd$bdata),
                         error = function(e) {
                             message("!! error caught in buildBench !!\n",
                                     "!! error in method: ", names(bd$methods)[i], "\n",
                                     "!!  original message: \n",
                                     "!!  ", e)
                             return(NA)
                         })
                 })
    names(al) <- names(bd$methods)
    al
}


## helper function to evaluate using BiocParallel
evalMethodsParallel <- function(bd, BPPARAM) {
    al <- bplapply(seq(bd$methods),
                   function(i) {
                       x <- bd$methods[[i]]
                       expr <- quo(UQ(x$func)(!!! x$params))
                       if (is.function(eval_tidy(x$post, bd$bdata))) {
                           expr <- quo(UQ(x$post)(!! expr))
                       } else if (is.list(eval_tidy(x$post, bd$bdata))) {
                           epost <- eval_tidy(x$post, bd$bdata)
                           enames <- names(epost)
                           if (all(sapply(epost, is_function))) {
                               expr <- quo({
                                   z <- (!! expr)
                                   lapply(UQ(x$post), function(zz) { zz(z) })
                               })
                           }
                       }
                       tryCatch(
                           eval_tidy(expr, bd$bdata),
                           error = function(e) {
                               message("!! error caught in buildBench !!\n",
                                       "!! error in method: ", names(bd$methods)[i], "\n",
                                       "!!  original message: \n",
                                       "!!  ", e)
                               return(NA)
                           })
                   },
                   BPPARAM = BPPARAM)
    names(al) <- names(bd$methods)
    al
}


## helper function to convert method info to character for colData
cleanMethods <- function(bd, ptabular) {
    df <- mapply(cleanMethod, m = bd$methods, mname = names(bd$methods), 
                 MoreArgs = list(bdata = bd$bdata, ptabular = ptabular),
                 SIMPLIFY = FALSE)
    df <- dplyr::bind_rows(df)
    df$label <- names(bd$methods)
    ## rownames(df) <- names(bd$methods)
    df
}

cleanMethod <- function(m, mname, bdata, ptabular) {
    ## delay evalution of metadata information until buildBench
    m$meta <- eval_tidy(m$meta, bdata)

    ## check if `meta =` is specified for method
    if (!is.null(m$meta)) {
        if (!is(m$meta, "list") ||
            length(names(m$meta)) != length(m$meta) ||
            is.null(names(m$meta)) ||
            any(names(m$meta) == "")) {
            warning(paste0("meta parameter specified for method '",
                           mname, "' will not be used. ",
                           "meta must be a list of named entries."))
            m$meta <- NULL
        }
    }

    ## parse package/version information
    meta <- funcMeta(m$func, m$meta)
    
    ## parse primary method
    if (meta$func_anon[1]) {
        func <- gsub("\n", ";", quo_text(m$func))
    } else {
        func <- quo_text(m$func)
    }
    
    ## parse postprocessing method
    has_post <- is.function(eval_tidy(m$post, bdata))
    has_postlist <- is.list(eval_tidy(m$post, bdata))
    if (has_post & !has_postlist) {
        post <- gsub("\n", ";", quo_text(m$post))
    } else if (has_postlist) {
        post <- paste(names(eval_tidy(m$post, bdata)), collapse = ";")
    } else {
        post <- NA_character_
    }
    
    res <- cbind(data.frame(func, post, stringsAsFactors = FALSE), meta)

    ## parse method parameters
    if (length(m$params) > 0) {
        if (ptabular) {
            bparams <- sapply(m$params, quo_text)
            names(bparams) <- paste0("param.", names(bparams))
            bparams <- data.frame(t(bparams), stringsAsFactors = FALSE)
        } else {
            bparams <- paste(names(m$params), "=",
                             sapply(m$params, quo_text),
                             collapse=", ")
            bparams <- data.frame(bparams, stringsAsFactors = FALSE)
        }
        res <- cbind(res, bparams)
    }

    res
}


## helper to gather important information for function
funcMeta <- function(f, meta) {

    ## determine if main `func` was anonymous
    ## -- If an anonymous func was specified directly to *Method, then the
    ##    capturing envirnment of the anonymous function will be a child of the
    ##    SummarizedBenchmark environment.
    ##    Handle this by greping for a 
    f_anon <- is.null(packageName(environment(eval_tidy(f)))) ||
        grepl("^\\s*function\\s*\\(", quo_text(f))

    fsrc <- eval_tidy(f)
    vers_src <- "func"
    if ("pkg_func" %in% names(meta)) {
        vers_src <- "meta_func"
        fsrc <- eval_tidy(rlang::as_quosure(meta$pkg_func))
    } else if ("pkg_name" %in% names(meta) |
               "pkg_name" %in% names(meta)) {
        pkg_name <- ifelse("pkg_name" %in% names(meta),
                           meta$pkg_name, NA_character_)
        pkg_vers <- ifelse("pkg_vers" %in% names(meta),
                           meta$pkg_vers, NA_character_)
        vers_src <- "meta_manual"
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

    res <- data.frame(func_anon = f_anon, vers_src = vers_src,
                      pkg_name = pkg_name, pkg_vers = pkg_vers,
                      stringsAsFactors = FALSE)

    ## need to parse and merge manually defined metadata columns 
    if (!is.null(meta)) {
        meta_func <- meta[["pkg_func"]] 
        meta <- meta[!(names(meta) %in% c("pkg_func", "pkg_name", "pkg_vers"))]
        if (length(meta) > 0) {
            names(meta) <- paste0("meta.", names(meta))
        }
        if (!is.null(meta_func)) {
            meta_func <- gsub("\n", ";", quo_text(meta_func))
            if (nchar(meta_func) > 100) {
                meta_func <- paste0(substr(meta_func, 1, 97), "...")
            }
            meta <- c(meta, "pkg_func" = meta_func)
        }
        if (length(meta) > 0) {
            res <- cbind(res, meta, stringsAsFactors = FALSE)
        }
    }

    res
}




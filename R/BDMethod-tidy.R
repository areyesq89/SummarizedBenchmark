#' Tidy BDMethod Data 
#'
#' A helper function to create tabular info for a single
#' BDMethod object or a list of BDMethod objects.
#'
#' @param bdm a BDMethod object
#' @param dat optional data object to use when evaluating any
#'        unevaluated expressionsin \code{bdm} meta data.
#'        (default = NULL)xo
#'
#' @return
#' A named vector of meta data for the specified BDMethod object, or
#' a tibble of meta data if a list of BDMethod objects specified.
#'
#' @export
#' @importFrom rlang eval_tidy
#' @author Patrick Kimes
setGeneric("tidyBDMethod",
           function(obj, dat = NULL, ...) standardGeneric("tidyBDMethod"))

.tidyBDMethod.bdm <- function(obj, dat) {
    obj@meta <- lapply(obj@meta, rlang::eval_tidy, data = dat)
    
    tidyp <- tidyBDMethodParams(obj@params)
    tidymf <- tidyBDMethodMeta(obj@meta)
    tidym <- tidymf$tidym

    if (is.null(tidymf$tidyf)) {
        tidyf <- tidyBDMethodFunction(obj@f)
        tidyf$func.pkg.manual <- FALSE
    } else {
        tidyf <- tidymf$tidyf
        tidyf$func.pkg.manual <- TRUE
    }

    c(tidyf, tidyp, tidym)
}

.tidyBDMethod.list <- function(obj, dat, label = FALSE) {
    df <- lapply(obj, tidyBDMethod, dat = dat)
    dplyr::bind_rows(df, .id = if(label) { "label" } else { NULL })
}

.tidyBDMethod.bd <- function(obj, dat, label = FALSE) {
    tidyBDMethod(obj@methods, dat, label)
}

#' @rdname tidyBDMethod
setMethod("tidyBDMethod", signature(obj = "BDMethod"), .tidyBDMethod.bdm)
setMethod("tidyBDMethod", signature(obj = "list"), .tidyBDMethod.list)
setMethod("tidyBDMethod", signature(obj = "BenchDesign"), .tidyBDMethod.bd)


tidyBDMethodFunction <- function(f) {
    fenv <- environment(f)
    pkgname <- packageName(fenv)

    if (is.null(pkgname)) {
        pkgname <- NA_character_
        pkgvers <- NA_character_
    } else {
        pkgvers <- as(packageVersion(pkgname), "character")
    }        

    list(func.pkg = pkgname, func.pkg.vers = pkgvers)
}


tidyBDMethodParams <- function(params) {
    if (length(params) ==  0) {
        return(NULL)
    }

    params <- sapply(params, quo_text)
    names(params) <- paste0("param.", names(params))
    params
}


tidyBDMethodMeta <- function(meta) {
    meta_pkgfunc <- meta[["pkg_func"]]
    meta_pkgname <- meta[["pkg_name"]] 
    meta_pkgvers <- meta[["pkg_vers"]] 

    if (!is.null(meta_pkgfunc)) {
        tidyf <- tidyBDMethodFunction(meta_pkgfunc)
    } else if (!is.null(meta_pkgname) | !is.null(meta_pkgvers)) {
        tidyf <- list(func.pkg = meta_pkgname, func.pkg.vers = meta_pkgvers)
    } else {
        tidyf <- NULL
    }

    meta <- meta[!(names(meta) %in% c("pkg_func", "pkg_name", "pkg_vers"))]
    if (length(meta) > 0) {
        names(meta) <- paste0("meta.", names(meta))
    }

    list(tidym = meta, tidyf = tidyf)
}

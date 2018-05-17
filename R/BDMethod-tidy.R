## Compare BDMethod meta data
#'
#' Simple comparison of two BDMethod objects based on
#' meta data.
#' 
#' @param bdm1 a \code{BDMethod} object
#' @param bdm2 a \code{BDMethod} object
#'
#' @return
#' logical value indicating whether two objects produced same
#' meta data.
#'
#' @export
#' @importFrom dplyr all_equal
#' @author Patrick Kimes
compareBDMethod <- function(bdm1, bdm2) {
    if(!is(bdm1, "BDMethod") || !is(bdm2, "BDMethod"))
        stop("Must specify two BDMethod objects to compare.")
    dplyr::all_equal(tidyBDMethod(bdm1), tidyBDMethod(bdm2))
}


## helper function to convert method info to character for colData
tidyBDMethods <- function(bdms, dat = NULL) {
    df <- lapply(bdms, tidyBDMethod, dat = dat)
    dplyr::bind_rows(df)
}


#' Tidy BDMethod Data 
#'
#' A helper function to create tabular info for a single
#' BDMethod object.
#'
#' @param bdm a BDMethod object
#' @param dat optional data object to use when evaluating any
#'        unevaluated expressionsin \code{bdm} meta data.
#'        (default = NULL)
#'
#' @return
#' A named vector of meta data for the specified BDMethod.
#'
#' @importFrom rlang eval_tidy
#' @author Patrick Kimes
tidyBDMethod <- function(bdm, dat = NULL) {
    if(!is(bdm, "BDMethod"))
        stop("Must specify a BDMethod object.")
    bdm@meta <- lapply(bdm@meta, rlang::eval_tidy, data = dat)
    
    tidyp <- tidyBDMethodParams(bdm@params)
    tidymf <- tidyBDMethodMeta(bdm@meta)
    tidym <- tidymf$tidym

    if (is.null(tidymf$tidyf)) {
        tidyf <- tidyBDMethodFunction(bdm@f)
        tidyf$func.pkg.manual <- FALSE
    } else {
        tidyf <- tidymf$tidyf
        tidyf$func.pkg.manual <- TRUE
    }

    c(tidyf, tidyp, tidym)
}


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

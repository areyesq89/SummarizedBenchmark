#' Tidy BDMethod Data 
#'
#' A helper function to extract information for a single
#' or multiple BDMethod object or a list of BDMethod objects.
#'
#' @param obj BDMethod object, list/List of BDMethod objects (e.g. a
#'        BDMethodList), or a BenchDesign object 
#' @param dat optional data object to use when evaluating any
#'        unevaluated expressionsin \code{bdm} meta data.
#'        (default = NULL)
#' @param eval logical whether to evaluate any quosures in the meta slot
#'        of the BDMethod objects. (default = FALSE)
#' @param label logical whether to add a "label" column to the resulting
#'        table containing the names of the methods if \code{obj} was
#'        specified as a named list. (default = FALSE)
#' 
#' @return
#' A named vector of meta data if only a single BDMethod object specified, else
#' a tibble of meta data for the specified list of methods.
#'
#' @details
#' If any quosures are specified to the "meta" slot of a BDMethod object,
#' the quosure is converted to a text string using \code{rlang::quo_text}.
#'
#' @name tidyBDMethod
#' @import dplyr
#' @importFrom rlang eval_tidy quo_text is_quosure
#' @author Patrick Kimes
NULL

.tidyBDMethod.bdm <- function(obj, dat, eval) {
    if (eval) {
        obj@meta <- lapply(obj@meta, function(x) {
            tryCatch(rlang::eval_tidy(x, data = dat),
                     error = function(e) {
                         message("!! error caught while trying to evaluate BDMethod meta slot !!\n",
                                 "!!  original message: \n",
                                 "!!  ", conditionMessage(e))
                         return(rlang::quo_text(x))
                     })
        })
    } else {
        quo_meta <- unlist(lapply(obj@meta, rlang::is_quosure))
        if (any(quo_meta))
            obj@meta[quo_meta] <- lapply(obj@meta[quo_meta], rlang::quo_text)
    }
    
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

.tidyBDMethod.list <- function(obj, dat, eval, label) {
    df <- lapply(obj, tidyBDMethod, dat = dat, eval = eval)
    dplyr::bind_rows(df, .id = if(label) { "label" } else { NULL })
}

.tidyBDMethod.bd <- function(obj, dat, eval, label) {
    if (!is.null(dat))
        tidyBDMethod(obj@methods, dat, eval, label)
    else if (!is.null(obj@data) && obj@data@type == "data")
        tidyBDMethod(obj@methods, obj@data@data, eval, label)
    else
        tidyBDMethod(obj@methods, NULL, eval, label)
}

#' @rdname tidyBDMethod
#' @export
setMethod("tidyBDMethod", signature(obj = "BDMethod"), .tidyBDMethod.bdm)

#' @rdname tidyBDMethod
#' @export
setMethod("tidyBDMethod", signature(obj = "list"), .tidyBDMethod.list)

#' @rdname tidyBDMethod
#' @export
setMethod("tidyBDMethod", signature(obj = "SimpleList"), .tidyBDMethod.list)

#' @rdname tidyBDMethod
#' @export
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

## helper function to compare bench design methods
compareBDMethod <- function(bdm1, bdm2) {
    all_equal(tidyBDMethod(bdm1), tidyBDMethod(bdm2))
}


## helper function to create tabular info for single method
tidyBDMethod <- function(bdm, dat = NULL) {
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
        tidyf <- c(func.pkg = meta_pkgname, func.pkg.vers = meta_pkgvers)
    } else {
        tidyf <- NULL
    }

    meta <- meta[!(names(meta) %in% c("pkg_func", "pkg_name", "pkg_vers"))]
    if (length(meta) > 0) {
        names(meta) <- paste0("meta.", names(meta))
    }

    list(tidym = meta, tidyf = tidyf)
}

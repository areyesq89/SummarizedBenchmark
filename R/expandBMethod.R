#' Expand method in BenchDesign object across parameter settings
#'
#' This function takes a BenchDesign object and the name of a method
#' already defined in the object, and returns a modified BenchDesign
#' object with multiple copies of the method differing by the specified
#' parameter sets.
#' 
#' @param b BenchDesign object.
#' @param blabel Character name of method to be modified.
#' @param param Character name of parameter to be modified. Should only be
#'        specified if only one parameter should be replaced in original
#'        method definition. Only one of `param` or `list` should be
#'        specified. (default = NULL)
#' @param ... If `param != NULL`, named values to use for
#'        overwriting the specified parameter in the original method definition.
#'        If `param = NULL`, then named lists of parameter sets to be used in
#'        place of parameters in the original method definition. Each parameter set
#'        should also be a named list of parameter, value pairs.
#'        Names will be used as the new method names in the BenchDesign object.
#'        An error will be returned if an existing method name is used.
#'        (defualt = NULL) 
#' @param .replace Logical whether original `blabel` object should be removed
#'        if method expansion is successful. (default = FALSE)
#' @param .overwrite Logical whether to overwrite the existing list of
#'        parameters (TRUE) or to simply add the new parameters to the existing
#'        list (FALSE). (default = FALSE) 
#' 
#' @return
#' Modified BenchDesign object.
#'
#' @export
#' @author Patrick Kimes
expandBMethod <- function(b, blabel, param = NULL, ...,
                          .replace = FALSE, .overwrite = FALSE) {
    UseMethod("expandBMethod")
}

#' @export
expandBMethod.BenchDesign <- function(b, blabel, param = NULL, ...,
                                      .replace = FALSE, .overwrite = FALSE) { 
    ## capture new parameter sets
    qd <- quos(...)

    ## verify that parameter names are valid
    if (is.null(names(qd))) {
        stop("New parameter values must be named.")
    }
    if (any(names(qd) %in% names(b$methods))) {
        stop("New method names should not overlap with names of current methods.")
    }
    if (any(duplicated(names(qd)))) {
        stop("New method names must be unique.")
    }
    
    ## verify that method definition already exists
    if(!(blabel %in% names(b$methods))) {
        stop("Specified method is not defined in BenchDesign.")
    }
    bm <- b$methods[[blabel]]
    
    ## expand differently based on whether param is specified
    if (is.null(param)) {
        if (.overwrite) {
            zl <- lapply(1:length(qd), function(zi) {
                qdqi <- lapply(lang_args(qd[[zi]]), as_quosure)
                ## ###################
                if ("bfunc" %in% names(qdqi)) {
                    bm$func <- qdqi$bfunc
                }
                if ("bpost" %in% names(qdqi)) {
                    bm$post <- qdqi$bpost
                }
                if ("bmeta" %in% names(qdqi)) {
                    bm$meta <- eval_tidy(qdqi$bmeta)
                }
                qdqi <- qdqi[! names(qdqi) %in% c("bfunc", "bpost", "bmeta")]
                ## ###################
                bm$dparams <- qdqi
                bm
            })
        } else {
            zl <- lapply(1:length(qd), function(zi) {
                qdqi <- lapply(lang_args(qd[[zi]]), as_quosure)
                ## ###################
                if ("bfunc" %in% names(qdqi)) {
                    bm$func <- qdqi$bfunc
                }
                if ("bpost" %in% names(qdqi)) {
                    bm$post <- qdqi$bpost
                }
                if ("bmeta" %in% names(qdqi)) {
                    bm$meta <- eval_tidy(qdqi$bmeta)
                }
                qdqi <- qdqi[! names(qdqi) %in% c("bfunc", "bpost", "bmeta")]
                ## ###################
                bm$dparams <- replace(bm$dparams, names(qdqi), qdqi)
                bm
            })
        }
    } else {
        zl <- lapply(1:length(qd), function(zi) {
            ## ###################
            if (param == "bfunc") {
                bm$func <- qd[[zi]]
            } else if (param == "bpost") {
                bm$post <- qd[[zi]]
            } else if (param == "bmeta") {
                bm$meta <- eval_tidy(qd[[zi]])
            } else {
                bm$dparams[[param]] <- qd[[zi]]
            }
            ## ###################
            bm
        })
    }
    names(zl) <- names(qd)

    ## drop source method
    if (.replace) {
        b$methods <- b$methods[names(b$methods) != blabel]
    }
    
    b$methods <- c(b$methods, zl)
    b
} 

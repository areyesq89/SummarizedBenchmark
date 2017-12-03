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
#' @param ... Named list of value to use for overwriting the specified
#'        parameter in the original method definition. Names will be
#'        used as the new method names in the BenchDesign object.  
#'        An error will be returned if an existing method name is used.
#'        (defualt = NULL) 
#' @param list Named list of parameter sets to be used in place of
#'        original method definition. Each parameter set should also be a
#'        named list of parameter, value pairs. All values which include a
#'        variable must be quoted using `rlang::quo`. Only one of `param` or `list`
#'        should be specified. Names of parameter sets will be used as the
#'        new method names in the BenchDesign object. An error will be returned 
#'        if an existing method name is used. (defualt = NULL)
#' @param replace Logical whether original `blabel` object should be removed
#'        if method expansion is successful. (default = FALSE)
#' @param overwrite Logical whether to overwrite the existing list of
#'        parameters (TRUE) or to simply add the new parameters to the existing
#'        list (FALSE). (default = FALSE) 
#'
#' @details
#' Currently, if the `list` approach is used, all variables must be quoted using
#' the `rlang::quo` function.
#' 
#' @return
#' Modified BenchDesign object.
#'
#' @export
#' @author Patrick Kimes
expandBMethod <- function(b, blabel, param = NULL, list = NULL, ...,
                          replace = FALSE, overwrite = FALSE) {
    UseMethod("expandBMethod")
}

expandBMethod.BenchDesign <- function(b, blabel, param = NULL, list = NULL, ...,
                                      replace = FALSE, overwrite = FALSE) { 
    ## capture input
    qd <- quos(...)
    
    ## verify that method definition already exists
    if(!(blabel %in% names(b$methods))) {
        stop("Specified method is not defined in BenchDesign.")
    }
    
    bm <- b$methods[[blabel]]

    ## verify that exactly one of 'param' or 'list' was specified
    if (!is.null(param) && !is.null(list)) {
        stop("Only one of 'param' or 'list' should be specified.")
    }
    if (is.null(param) && is.null(list)) {
        stop("One of 'param' or 'list' should be specified.")
    }
    
    ## expand differently depending on 
    if (!is.null(param)) {
        if (is.null(names(qd))) {
            stop("New parameter values must be named.")
        }
        if (any(names(qd) %in% names(b$methods))) {
            stop("New method names should not overlap with names of current methods.")
        }
        if (any(duplicated(names(qd)))) {
            stop("New method names must be unique.")
        }
        
        zl <- lapply(1:length(qd), function(zi) {
            bm$dparams[[param]] <- qd[[zi]]
            bm
        })
        names(zl) <- names(qd)
        
    } else {
        if (is.null(names(list))) {
            stop("New parameter sets must be named.")
        }
        if (any(names(list) %in% names(b$methods))) {
            stop("New method names should not overlap with names of current methods.")
        }
        if (any(duplicated(names(list)))) {
            stop("New method names must be unique.")
        }

        if (overwrite) {
            zl <- lapply(1:length(list), function(zi) {
                bm$dparams <- list[[zi]]
                bm
            })
        } else {
            zl <- lapply(1:length(list), function(zi) {
                bm$dparams <- replace(bm$dparams,
                                      names(list[[zi]]), list[[zi]])
                bm
            })
        }
        names(zl) <- names(list)
    }

    ## drop source method
    if (replace) {
        b$methods <- b$methods[names(b$methods) != blabel]
    }
    
    b$methods <- c(b$methods, zl)
    b
} 

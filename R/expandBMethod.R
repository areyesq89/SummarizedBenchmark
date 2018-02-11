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
#' @examples
#' ## with toy data.frame
#' df <- data.frame(pval = rnorm(100))
#' bd <- BenchDesign(df)
#'
#' ## add basic 'padjust' method
#' bd <- addBMethod(bd, blabel = "padjust", bfunc = p.adjust,
#'                  p = pval)
#'
#' ## "expand" 'padjust' by adding "method" parameters
#' bd <- expandBMethod(bd, blabel = "padjust",
#'                     param = "method",
#'                     bonf = "bonferonni", BH = "BH",
#'                     .replace = TRUE)
#'
#' ## resulting BenchDesign has same methods as following set of calls
#' bd_alt <- BenchDesign(df)
#' bd <- addBMethod(bd_alt, blabel = "bonf", bfunc = p.adjust,
#'                  p = pval, method = "bonferroni")
#' bd <- addBMethod(bd_alt, blabel = "BH", bfunc = p.adjust,
#'                  p = pval, method = "BH")
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
    if (any(nchar(names(qd)) == 0)) {
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

    ## convert new params to named list of quosures
    new_names <- names(qd)
    if (is.null(param)) {
        qd <- lapply(1:length(qd), function(zi) {
            lapply(lang_args(qd[[zi]]), as_quosure)
        })
    } else {
        qd <- lapply(1:length(qd), function(zi) {
            qdqi <- quos(!! param := !! qd[[zi]])
        })
    }
    
    ## handle all using same named list format
    zl <- lapply(qd, .modmethod, m = bm, .overwrite = .overwrite)
    names(zl) <- new_names

    ## drop source method
    if (.replace) {
        b$methods <- b$methods[names(b$methods) != blabel]
    }
    
    b$methods <- c(b$methods, zl)
    b
} 

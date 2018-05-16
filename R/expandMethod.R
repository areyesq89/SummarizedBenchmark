#' Expand method in BenchDesign object across parameter settings
#'
#' This function takes a BenchDesign object and the name of a method
#' already defined in the object, and returns a modified BenchDesign
#' object with multiple variants of the method differing only by the
#' specified parameter sets. In other words, this function "expands"
#' the set of methods using a set of parameters. 
#' 
#' @param bd BenchDesign object.
#' @param label Character name of method to be expanded.
#' @param params Named list of quosure lists specifying the label of the 
#'        new methods to be added to the BenchDesign, and the set of
#'        parameters to overwrite in the original method definition for
#'        each new method. Alternatively, if `onlyone` is non-NULL, a single quosure
#'        list with `name = value` pairs specifying the label of the new methods and
#'        the values to use for overwriting the parameter specified in `onlyone`.
#' @param onlyone Character name of a parameter to be modified. Only specify
#'        if just a single parameter should be replaced in the original
#'        method definition. Ignored if NULL. (default = NULL)
#' @param .replace Logical whether original `label` object should be removed
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
#' bench <- BenchDesign(df)
#'
#' ## add basic 'padjust' method
#' bench <- addMethod(bench, label = "padjust",
#'                    func = p.adjust,
#'                    params = rlang::quos(p = pval,
#'                                         method = "none"))
#'
#' ## modify multiple parameters, "p" and "method"
#' bench_exp <- expandMethod(bench, label = "padjust",
#'                           params = list(
#'         bonf = rlang::quos(p = round(pval, 5),
#'                            method = "bonferonni"),
#'         bh = rlang::quos(p = round(pval, 3),
#'                          method = "BH")))
#' printMethods(bench_exp)
#'
#' ## only modify a single parameter using the 'onlyone=' parameter
#' bench_exp <- expandMethod(bench, label = "padjust",
#'                           onlyone = "method",
#'                           params = rlang::quos(bonf = "bonferonni",
#'                                                BH = "BH"))
#' printMethods(bench_exp)
#'
#' @md
#' @importFrom rlang is_quosures quos !! :=
#' @export
#' @author Patrick Kimes
expandMethod <- function(bd, label, params, onlyone = NULL,
                         .replace = FALSE, .overwrite = FALSE) {
    UseMethod("expandMethod")
}

#' @export
expandMethod.BenchDesign <- function(bd, label, params, onlyone = NULL, 
                                     .replace = FALSE, .overwrite = FALSE) {
    ## parse new inputs
    if (rlang::is_quosures(params)) {
        if (is.null(onlyone)) {
            stop("If onlyone = NULL, a named list of parameter lists must be supplied to ",
                 "'params =' as a list of quosure lists created using rland::quos.\n",
                 "e.g. params = list(new_method1 = quos(..), new_method2 = quos(..))")
        }
        qd <- lapply(1:length(params), function(zi) {
            rlang::quos(!! onlyone := !! params[[zi]])
        })
        names(qd) <- names(params)
    } else if (is.list(params) & all(sapply(params, rlang::is_quosures))) {
        if (!is.null(onlyone)) {
            stop("If onlyone is non-NULL, a list of parameter values must be supplied to ",
                 "'params =' as a list of quosures created using rland::quos.\n",
                 "e.g. params = quos(new_method1 = new_value1, new_method2 = new_value2)")
        }
        qd <- params
    } else {
        if (is.null(onlyone)) {
            stop("If 'onlyone = NULL', a named list of parameter lists must be supplied to ",
                 "'params =' as a list of quosure lists created using rland::quos.\n",
                 "e.g. params = list(new_method1 = quos(..), new_method2 = quos(..))")
        } else {
            stop("If 'onlyone' is non-NULL, a list of parameter values must be supplied to ",
                 "'params =' as a list of quosures created using rland::quos.\n",
                 "e.g. params = quos(new_method1 = new_value1, new_method2 = new_value2)")
        }
    }

    ## verify that parameter names are valid
    if (is.null(names(qd)) | any(nchar(names(qd)) == 0)) {
        stop("New parameter values must be named.")
    }
    if (any(names(qd) %in% names(bd@methods))) {
        stop("New method names should not overlap with names of current methods.")
    }
    if (any(duplicated(names(qd)))) {
        stop("New method names must be unique.")
    }
    
    ## verify that method definition already exists
    if(!(label %in% names(bd@methods))) {
        stop("Specified method is not defined in BenchDesign.")
    }
    bm <- bd@methods[[label]]
    
    ## handle all using same named list format
    zl <- lapply(qd, .modmethod, m = bm, .overwrite = .overwrite)
    names(zl) <- names(qd)

    ## drop source method
    if (.replace) {
        bd <- dropMethod(bd, label)
    }
    
    bd@methods <- c(bd@methods, zl)
    bd
} 

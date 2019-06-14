#' Expand method in BenchDesign object
#'
#' @description
#' Takes a \code{\link[=BenchDesign-class]{BenchDesign}} object, the name of an
#' existing method, and new parameter specifications, 
#' and returns a modified \code{\link[=BenchDesign-class]{BenchDesign}}
#' object with new methods added. The named method is "expanded" to mutliple methods
#' according to the specified set of parameters. 
#' 
#' @param bd \code{\link[=BenchDesign-class]{BenchDesign}} object.
#' @param label Character name of method to be expanded.
#' @param params Named list of quosure lists specifying the label of the 
#'        new methods to be added to the \code{\link[=BenchDesign-class]{BenchDesign}},
#'        and the set of
#'        parameters to overwrite in the original method definition for
#'        each new method. Alternatively, if `onlyone` is non-\code{NULL}, a single quosure
#'        list with `name = value` pairs specifying the label of the new methods and
#'        the values to use for overwriting the parameter specified in `onlyone`.
#' @param onlyone Character name of a parameter to be modified. Only specify
#'        if just a single parameter should be replaced in the original
#'        method definition. Ignored if \code{NULL}. (default = \code{NULL})
#' @param .replace Logical whether original `label` method should be removed.
#'        (default = \code{FALSE})
#' @param .overwrite Logical whether to overwrite the existing list of
#'        parameters (`TRUE`) or to simply add the new parameters to the existing
#'        list (`FALSE`). (default = `FALSE`) 
#' 
#' @return
#' Modified \code{\link[=BenchDesign-class]{BenchDesign}} object with new methods with
#' specified parameters added.
#'
#' @examples
#' ## empty BenchDesign
#' bench <- BenchDesign()
#'
#' ## add basic 'padjust' method
#' bench <- addMethod(bench, label = "padjust",
#'                    func = p.adjust,
#'                    params = rlang::quos(p = pval, method = "none"))
#'
#' ## modify multiple parameters - params is a list of quosure lists
#' newparams <- list(bonf = rlang::quos(p = round(pval, 5), method = "bonferonni"),
#'                   bh = rlang::quos(p = round(pval, 3), method = "BH"))
#' bench_exp <- expandMethod(bench, label = "padjust", params = newparams)
#' BDMethodList(bench_exp)
#'
#' ## only modify a single parameter - params is a quosure list
#' newparams <- rlang::quos(bonf = "bonferonni", BH = "BH")
#' bench_exp <- expandMethod(bench, label = "padjust", onlyone = "method", params = newparams)
#' BDMethodList(bench_exp)
#'
#' @seealso \code{\link{modifyMethod}}, \code{\link{addMethod}}, \code{\link{dropMethod}}
#' @md
#' @importFrom rlang is_quosures quos
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
                 "'params =' as a list of quosure lists created using rlang::quos.\n",
                 "e.g. params = list(new_method1 = quos(..), new_method2 = quos(..))")
        }
        qd <- lapply(1:length(params), function(zi) {
            rlang::quos(!! onlyone := !! params[[zi]])
        })
        names(qd) <- names(params)
    } else if (is.list(params) & all(sapply(params, rlang::is_quosures))) {
        if (!is.null(onlyone)) {
            stop("If onlyone is non-NULL, a list of parameter values must be supplied to ",
                 "'params =' as a list of quosures created using rlang::quos.\n",
                 "e.g. params = quos(new_method1 = new_value1, new_method2 = new_value2)")
        }
        qd <- params
    } else {
        if (is.null(onlyone)) {
            stop("If 'onlyone = NULL', a named list of parameter lists must be supplied to ",
                 "'params =' as a list of quosure lists created using rlang::quos.\n",
                 "e.g. params = list(new_method1 = quos(..), new_method2 = quos(..))")
        } else {
            stop("If 'onlyone' is non-NULL, a list of parameter values must be supplied to ",
                 "'params =' as a list of quosures created using rlang::quos.\n",
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

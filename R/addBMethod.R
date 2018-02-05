#' Add new method to BenchDesign object
#'
#' This function takes a BenchDesign object and returns a
#' modified object with the specified method included. At a
#' minimum, a string name for the method, `blabel`, and the
#' workhorse function for the method, `bfunc`, must be specified
#' in addition to the primary BenchDesign object.
#'
#' The inputs for the call to `blabel` should be specified as
#' `parameter = value` pairs, where the `value` can be any
#' fixed value, variable, or column in the `bdata` of the
#' BenchDesign object.
#'
#' An optional secondary function, `bpost`, can be specified if
#' the output of the workhorse function, `bfunc`, needs to be
#' further processed. As an example, `bpost` may be a simple
#' "getter" function for accessing the column of interest from
#' the large object returned by `bfunc`.
#' 
#' @param b BenchDesign object.
#' @param blabel Character name for the method.
#' @param bfunc Primary function to be benchmarked.
#' @param bpost Optional post-processing function that takes
#'        results of `bfunc` as input. Ignored if NULL.
#'        If multiple assays (metrics) should be generated for each
#'        method, this can be accomplished by specifying a named
#'        list of post-processing functions, one for each assay. 
#'        (default = NULL)
#' @param bmeta Optional metadata information for method to be
#'        included in `colData` of `SummarizedBenchmark` object
#'        generated using `buildBench`. See Details for more
#'        information. Ignored if NULL. (default = NULL)
#' @param ... Named `parameter = value` pairs to be passed to
#'        `bfunc`.
#'
#' @return
#' A copy of the originally supplied BenchDesign with the
#' new method added.
#' 
#' @details
#' The optional `bmeta` parameter accepts a named list of metadata
#' tags to be included for the method in the resulting `SummarizedBenchmark`
#' object. This can be useful for two primary cases. First, it can help keep
#' analyses better organized by allowing the specification of additional
#' information that should be stored with methods, e.g. a tag for "method type"
#' or descriptive information on why the method was included in the comparison.
#' Second, and more improtantly, the `bmeta` parameter can be used to overwrite
#' the package and version information that is automatically extracted from the
#' function specified to `bfunc`. This is particularly useful when the function
#' passed to `bfunc` is a wrapper for a script in (or outside of) R, and the
#' appropriate package and version information can't be directly pulled from
#' `bfunc`. In this case, the user can either manually specify the `"pkg_name"`
#' and `"pkg_vers"` values to `bmeta` as a list, or specify a separate function
#' that should be used to determine the package name and version. If a separate
#' function should be used, it should be passed to `bmeta` as a list entry
#' with the name `pkg_func` and first quoted using `rlang::quo`, e.g.
#' `list(pkg_func = quo(p.adjust))`.
#' 
#' @examples
#' ## create example data set of p-values
#' df <- data.frame(pval = runif(100))
#'
#' ## example calculating qvalue from pvalues
#'
#' ## using standard call
#' qv <- qvalue::qvalue(p = sim_df$pval)
#' qv <- qv$qvalue
#'
#' ## adding same method to BenchDesign
#' bd <- BenchDesign(df)
#' bd <- addBMethod(bd, "qv",
#'                  bfunc = qvalue::qvalue,
#'                  bpost = function(x) { x$qvalue },
#'                  p = pval)
#'
#' @md
#' @import rlang
#' @export
#' @author Patrick Kimes
addBMethod <- function(b, blabel, bfunc, bpost = NULL, bmeta = NULL, ...) {
    UseMethod("addBMethod")
}

#' @export
addBMethod.BenchDesign <- function(b, blabel, bfunc, bpost = NULL, bmeta = NULL, ...) {
    ## capture input
    qf <- enquo(bfunc)
    qd <- quos(...)
    qp <- enquo(bpost)
    qm <- enquo(bmeta)
    
    ## add to bench
    b$methods[[blabel]] <- list(func = qf, dparams = qd, 
                                post = qp, meta = qm)
    b
}

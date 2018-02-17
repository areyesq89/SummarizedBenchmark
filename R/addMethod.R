#' Add new method to BenchDesign object
#'
#' This function takes a BenchDesign object and returns a
#' modified object with the specified method included. At a
#' minimum, a string name for the method, `label`, and the
#' workhorse function for the method, `func`, must be specified
#' in addition to the primary BenchDesign object.
#'
#' The inputs for the call to `label` should be specified as
#' `parameter = value` pairs, where the `value` can be any
#' fixed value, variable, or column in the `bdata` of the
#' BenchDesign object.
#'
#' An optional secondary function, `post`, can be specified if
#' the output of the workhorse function, `func`, needs to be
#' further processed. As an example, `post` may be a simple
#' "getter" function for accessing the column of interest from
#' the large object returned by `func`.
#' 
#' @param bd BenchDesign object.
#' @param label Character name for the method.
#' @param func Primary function to be benchmarked.
#' @param params Named quosure list created using `rlang::quos` of
#'        `parameter = value` pairs to be passed to `func`.
#' @param post Optional post-processing function that takes
#'        results of `func` as input. Ignored if NULL.
#'        If multiple assays (metrics) should be generated for each
#'        method, this can be accomplished by specifying a named
#'        list of post-processing functions, one for each assay. 
#'        (default = NULL)
#' @param meta Optional metadata information for method to be
#'        included in `colData` of `SummarizedBenchmark` object
#'        generated using `buildBench`. See Details for more
#'        information. Ignored if NULL. (default = NULL)
#'
#' @return
#' A copy of the originally supplied BenchDesign with the
#' new method added.
#' 
#' @details
#' The optional `meta` parameter accepts a named list of metadata
#' tags to be included for the method in the resulting `SummarizedBenchmark`
#' object. This can be useful for two primary cases. First, it can help keep
#' analyses better organized by allowing the specification of additional
#' information that should be stored with methods, e.g. a tag for "method type"
#' or descriptive information on why the method was included in the comparison.
#' Second, and more improtantly, the `meta` parameter can be used to overwrite
#' the package and version information that is automatically extracted from the
#' function specified to `func`. This is particularly useful when the function
#' passed to `func` is a wrapper for a script in (or outside of) R, and the
#' appropriate package and version information can't be directly pulled from
#' `func`. In this case, the user can either manually specify the `"pkg_name"`
#' and `"pkg_vers"` values to `meta` as a list, or specify a separate function
#' that should be used to determine the package name and version. If a separate
#' function should be used, it should be passed to `meta` as a list entry
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
#' qv <- qvalue::qvalue(p = df$pval)
#' qv <- qv$qvalue
#'
#' ## adding same method to BenchDesign
#' bench <- BenchDesign(df)
#' bench <- addMethod(bench,
#'                    label = "qv",
#'                    func = qvalue::qvalue,
#'                    post = function(x) { x$qvalue },
#'                    params = rlang::quos(p = pval))
#'
#' @md
#' @importFrom rlang enquo quo quos is_quosures
#' @export
#' @author Patrick Kimes
addMethod <- function(bd, label, func, params = rlang::quos(),
                      post = NULL, meta = NULL) {
    UseMethod("addMethod")
}

#' @export
addMethod.BenchDesign <- function(bd, label, func, params = rlang::quos(),
                                  post = NULL, meta = NULL) {
    if (!rlang::is_quosures(params)) {
        stop("Please supply 'func' parameters to 'params =' as ",
             "a list of quosures using rlang::quos.\n",
             "e.g. params = quos(param1 = x, param2 = y)")
    }
    ## capture input
    qf <- rlang::enquo(func)
    qp <- rlang::enquo(post)
    qm <- rlang::enquo(meta)
    
    ## add to bench
    bd$methods[[label]] <- list(func = qf, params = params, 
                                post = qp, meta = qm)
    bd
}

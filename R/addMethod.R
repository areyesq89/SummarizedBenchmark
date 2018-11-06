#' Add method to BenchDesign object
#'
#' @description
#' Takes a \code{\link[=BenchDesign-class]{BenchDesign}} object
#' and the definition of a new method for benchmarking and returns
#' the original \code{\link[=BenchDesign-class]{BenchDesign}} with
#' the new method included.
#'
#' At a minimum, a method label (`label =`), and the
#' workhorse function for the method (`func =`) must be specified
#' for the new method. 
#'
#' Parameters for the method must be specified as a 
#' \code{\link[rlang]{quos}} named list of `parameter = value` pairs
#' mapping entries in the benchmarking data to the function parameters.
#' For users familiar with the \pkg{ggplot2} package, this can be
#' viewed similar to the \code{aes =} mapping of data to geometry
#' parameters.
#'
#' An optional secondary function, `post`, can be specified if
#' the output of the workhorse function, `func`, needs to be
#' further processed. As an example, `post` may be a simple
#' "getter" function for accessing the column of interest from
#' the large object returned by `func`.
#' 
#' @param bd \code{\link[=BenchDesign-class]{BenchDesign}} object.
#' @param label Character name for the method.
#' @param func Primary function to be benchmarked.
#' @param params Named quosure list created using \code{\link[rlang]{quos}} of
#'        `parameter = value` pairs to be passed to `func`.
#' @param post Optional post-processing function that takes
#'        results of `func` as input. Ignored if \code{NULL}.
#'        If multiple assays (metrics) should be generated for each
#'        method, this can be accomplished by specifying a named
#'        list of post-processing functions, one for each assay. 
#'        (default = \code{NULL})
#' @param meta Optional metadata information for method to be
#'        included in `colData` of \code{\link[=SummarizedBenchmark-class]{SummarizedBenchmark}} object
#'        generated using \code{link{buildBench}}. See Details for more
#'        information. Ignored if \code{NULL}. (default = \code{NULL})
#'
#' @return
#' Modified \code{\link[=BenchDesign-class]{BenchDesign}} object with
#' new method added.
#' 
#' @details
#' The optional `meta` parameter accepts a named list of metadata
#' tags to be included for the method in the resulting
#' \code{\link[=SummarizedBenchmark-class]{SummarizedBenchmark}}
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
#' with the name `pkg_func` and first quoted using \code{\link[rlang]{quo}}, e.g.
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
#' bench <- BenchDesign(data = df)
#' bench <- addMethod(bench,
#'                    label = "qv",
#'                     func = qvalue::qvalue,
#'                     post = function(x) { x$qvalue },
#'                     params = rlang::quos(p = pval))
#'
#' @seealso \code{\link{modifyMethod}}, \code{\link{expandMethod}}, \code{\link{dropMethod}}
#' @md
#' @importFrom rlang is_quosures quos enquo
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
    
    ## add to bench
    bd@methods[[label]] <- BDMethod(x = qf, params = params, post = post, meta = meta)
    bd
}

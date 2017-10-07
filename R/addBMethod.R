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
#'        output of `bfunc` as input. Ignored if NULL.
#'        (default = NULL)
#' @param ... Named `parameter = value` pairs to be passed to
#'        `func`.
#'
#' @examples
#' \dontrun{
#' ## assume sim_df is a data.frame with column: pval 
#' bd <- BenchDesign(sim_df)
#'
#' ## example calculating qvalue from pvalues
#'
#' ## using standard call
#' qv <- qvalue::qvalue(p = sim_df$pval)
#' qv <- qv$qvalue
#'
#' ## adding same method to BenchDesign
#' bd <- bd %>%
#'     addBMethod("qv",
#'                bfunc = qvalue::qvalue,
#'                bpost = function(x) { x$qvalue },
#'                p = pval)
#' }
#'
#' @md
#' @export
#' @author Patrick Kimes
addBMethod <- function(b, blabel, bfunc, bpost = NULL, ...) {
    UseMethod("addBMethod")
}

#' @export
addBMethod.BenchDesign <- function(b, blabel, bfunc, bpost = NULL, ...) {
    ## capture input
    qf <- enquo(bfunc)
    qd <- quos(...)
    qp <- enquo(bpost)
    
    ## add to bench
    b$methods[[blabel]] <- list(func = qf, dparams = qd, post = qp)
    b
}

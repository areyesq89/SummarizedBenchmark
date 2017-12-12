#' Modify definition of method in BenchDesign object
#'
#' This function takes a BenchDesign object and the name of a method
#' already defined in the object, and returns a modified BenchDesign
#' object with the specified changes made only to the named method.
#' At a minimum, a string name for the method, `blabel`, must be
#' specified in addition to the primary BenchDesign object.
#' 
#' @param b BenchDesign object.
#' @param blabel Character name of method to be modified.
#' @param ... Named parameter, value pairs to overwrite 
#'        in method definition. This can include `bfunc`,
#'        `bpost`, and `bmeta` parameters. All other named parameters
#'        will be added to the list of parameters to be passed to
#'        `bfunc`.
#' @param .overwrite Logical whether to overwrite the existing list of
#'        parameters to be passed to `bfunc` (TRUE), or to simply add
#'        the new parameters to the existing list (FALSE).
#'        (default = FALSE) 
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
#'                bmeta = list(note = "storey's q-value"),
#'                p = pval)
#'
#' ## modify method 'bmeta' property of 'qv' method
#' bd <- bd %>%
#'     modifyBMethod("qv",
#'                   bmeta = list(note = "Storey's q-value"))
#' 
#' ## verify that method has been updated
#' showBMethod(bd, "qv")
#' }
#'
#' @return
#' Modified BenchDesign object.
#' 
#' @md
#' @import rlang
#' @export
#' @author Patrick Kimes
modifyBMethod <- function(b, blabel, ..., .overwrite = FALSE) {
    UseMethod("modifyBMethod")
}

#' @export
modifyBMethod.BenchDesign <- function(b, blabel, ..., .overwrite = FALSE) {
    ## capture input
    qd <- quos(...)

    ## verify that method definition already exists
    if(!(blabel %in% names(b$methods))) {
        stop("Specified method is not defined in BenchDesign.")
    }

    bm <- b$methods[[blabel]]

    ## parse out bfunc, bpost, bmeta
    if ("bfunc" %in% names(qd)) {
        bm$func <- qd$bfunc
    }
    if ("bpost" %in% names(qd)) {
        bm$post <- qd$bpost
    }
    if ("bmeta" %in% names(qd)) {
        bm$meta <- eval_tidy(qd$bmeta)
    }

    ## process named parameters to be used for bfunc
    qd <- qd[! names(qd) %in% c("bfunc", "bpost", "bmeta")]
    if (.overwrite) {
        bm$dparams <- qd
    } else {
        bm$dparams <- replace(bm$dparams, names(qd), qd)
    }
        
    ## add to bench
    b$methods[[blabel]] <- bm

    return(b)
}

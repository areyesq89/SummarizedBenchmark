#' Modify definition of method in BenchDesign object
#'
#' This function takes a BenchDesign object and the name of a method
#' already defined in the object, and returns a modified BenchDesign
#' object with the specified changes made only to the named method.
#' At a minimum, a string name for the method, `label`, must be
#' specified in addition to the primary BenchDesign object.
#' 
#' @param bd BenchDesign object.
#' @param label Character name of method to be modified.
#' @param ... Named parameter, value pairs to overwrite 
#'        in method definition. This can include `func`,
#'        `post`, and `meta` parameters. All other named parameters
#'        will be added to the list of parameters to be passed to
#'        `func`.
#' @param .overwrite Logical whether to overwrite the existing list of
#'        parameters to be passed to `func` (TRUE), or to simply add
#'        the new parameters to the existing list (FALSE).
#'        (default = FALSE) 
#'        
#' @examples
#' ## with toy data.frame
#' df <- data.frame(pval = runif(100))
#' bench <- BenchDesign(df)
#'
#' ## add method
#' bench <- addMethod(bench, label = "qv",
#'                  func = qvalue::qvalue,
#'                  post = function(x) { x$qvalue },
#'                  meta = list(note = "storey's q-value"),
#'                  p = pval)
#'
#' ## modify method 'meta' property of 'qv' method
#' bench <- modifyMethod(bench, label = "qv",
#'                     meta = list(note = "Storey's q-value"))
#' 
#' ## verify that method has been updated
#' printMethod(bench, "qv")
#'
#' @return
#' Modified BenchDesign object.
#' 
#' @md
#' @import rlang
#' @export
#' @author Patrick Kimes
modifyMethod <- function(bd, label, ..., .overwrite = FALSE) {
    UseMethod("modifyMethod")
}

#' @export
modifyMethod.BenchDesign <- function(bd, label, ..., .overwrite = FALSE) {
    ## capture input
    qd <- quos(...)

    ## verify that method definition already exists
    if(!(label %in% names(bd$methods))) {
        stop("Specified method is not defined in BenchDesign.")
    }

    ## modify and add to bench
    bm <- bd$methods[[label]]
    bd$methods[[label]] <- .modmethod(bm, qd, .overwrite)

    return(bd)
}


#' Modify BenchDesign Method
#'
#' Given a method defined in a BenchDesign, this helper function
#' returns a modified method with new parameters defined as a
#' list of quosures.
#' 
#' @param m method
#' @param q quosure list of new parameters
#' @param .overwrite logical whether to overwrite parameters
#'
#' @return
#' modified method. 
#'
#' @rdname modmethod
#' @keywords internal
#' @author Patrick Kimes
.modmethod <- function(m, q, .overwrite) {
    ## parse out func, post, meta
    if ("func" %in% names(q)) {
        m$func <- q$func
    }
    if ("post" %in% names(q)) {
        m$post <- q$post
    }
    if ("meta" %in% names(q)) {
        m$meta <- eval_tidy(q$meta)
    }

    ## process named parameters to be used for func
    q <- q[! names(q) %in% c("func", "post", "meta")]
    if (.overwrite) {
        m$dparams <- q
    } else {
        m$dparams <- replace(m$dparams, names(q), q)
    }
    
    return(m)
}

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
#' @param params Named quosure list created using `rlang::quos` of 
#'        `parameter = value` paiars to replace in the method definition.
#'        The `post`, and `meta` parameters of the method can be
#'        modified using the special keywords, `bd.post`, and `bd.meta`
#'        (the prefix denoting that these values should modify BenchDesign
#'        parameters). All other named parameters will be added to the list of
#'        parameters to be passed to `func`.
#' @param .overwrite Logical whether to overwrite the complete existing list of
#'        parameters to be passed to `func` (TRUE), or to simply add
#'        the new parameters to the existing list and only replace overlapping
#'        parameters (FALSE). (default = FALSE) 
#'        
#' @examples
#' ## empty BenchDesign
#' bench <- BenchDesign()
#'
#' ## add method
#' bench <- addMethod(bench, label = "qv",
#'                    func = qvalue::qvalue,
#'                    post = function(x) { x$qvalue },
#'                    meta = list(note = "storey's q-value"),
#'                    params = rlang::quos(p = pval))
#'
#' ## modify method 'meta' property of 'qv' method
#' bench <- modifyMethod(bench, label = "qv",
#'                       params = rlang::quos(bd.meta =
#'                            list(note = "Storey's q-value")))
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
modifyMethod <- function(bd, label, params, .overwrite = FALSE) {
    UseMethod("modifyMethod")
}

#' @export
modifyMethod.BenchDesign <- function(bd, label, params, .overwrite = FALSE) {
    ## verify that method definition already exists
    if (!(label %in% names(bd@methods))) {
        stop("Specified method is not defined in BenchDesign.")
    }

    if (!rlang::is_quosures(params)) {
        stop("Please supply 'func' parameters to 'params =' as ",
             "a list of quosures using rlang::quos.\n",
             "e.g. params = quos(param1 = x, param2 = y)")
    }
    
    ## modify and add to bench
    bm <- bd@methods[[label]]
    bd@methods[[label]] <- .modmethod(bm, params, .overwrite)

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
    ## parse out post, meta
    if ("bd.post" %in% names(q)) {
        new_post <- rlang::eval_tidy(q$bd.post)
    } else {
        new_post <- m@post
    }
    if ("bd.meta" %in% names(q)) {
        new_meta <- rlang::eval_tidy(q$bd.meta)
    } else {
        new_meta <- m@meta
    }

    ## process named parameters to be used for func
    q <- q[! names(q) %in% c("bd.post", "bd.meta")]
    if (.overwrite) {
        new_params <- q
    } else {
        new_params <- replace(m@params, names(q), q)
    }

    ## easiest way to create modified BDMethod w/ m@f, m@fc
    bd <- BDMethod(f = m@f, post = new_post, meta = new_meta,
                   params = new_params)
    bd@fc <- m@fc
    bd
}

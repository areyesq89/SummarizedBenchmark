#' BDMethodList Accessors
#'
#' These methods can be used to access, set, and remove elements of
#' a BDMethodList object.
#' 
#' @param x BDMethodList object.
#' @param i character name or integer index of a BDMethod object.
#'        (Note: must be a character name for replacement operations.)
#' @param value replacement object, either BDMethod or NULL object.
#'
#' @return
#' modified BDMethodList object
#'
#' @name BDMethodList-accessors
#' @author Patrick Kimes
NULL

#' @rdname BDMethodList-accessors
#' @exportMethod "BDMethod<-"
setReplaceMethod("BDMethod",
                 signature(x = "BDMethodList", i = "character", value = "BDMethod"),
                 function (x, i, value) {
                     x[[i]] <- value
                     x
                 })

#' @rdname BDMethodList-accessors
#' @exportMethod "BDMethod<-"
setReplaceMethod("BDMethod",
                 signature(x = "BDMethodList", i = "character", value = "NULL"),
                 function (x, i, value) {
                     x[[i]] <- value
                     x
                 })

#' @rdname BDMethodList-accessors
#' @exportMethod "BDMethod"
setMethod("BDMethod",
          signature(x = "BDMethodList"),
          function(x, i = 1) {
              stopifnot(is(i, "character") || is(i, "numeric"))
              x[[i]]
          })

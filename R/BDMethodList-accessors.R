#' @rdname BDMethod-setter
#' @exportMethod "BDMethod<-"
setReplaceMethod("BDMethod",
                 signature(x = "BDMethodList", i = "character", value = "BDMethod"),
                 function (x, i, value) {
                     x[[i]] <- value
                     x
                 })

#' @rdname BDMethod-setter
#' @exportMethod "BDMethod<-"
setReplaceMethod("BDMethod",
                 signature(x = "BDMethodList", i = "character", value = "NULL"),
                 function (x, i, value) {
                     x[[i]] <- value
                     x
                 })

#' @rdname BDMethod
#' @exportMethod "BDMethod"
setMethod("BDMethod",
          signature(x = "BDMethodList"),
          function(x, i = 1) {
              stopifnot(is(i, "character") || is(i, "numeric"))
              x[[i]]
          })

#' @rdname BDData-setter
#' @exportMethod "BDData<-"
setReplaceMethod("BDData",
                 signature(x = "BenchDesign", value = "BDDataOrNULL"),
                 function (x, value) {
                     x@data <- value
                     x
                 })


#' @rdname BDMethod-setter
#' @exportMethod "BDMethod<-"
setReplaceMethod("BDMethod",
                 signature(x = "BenchDesign", i = "character", value = "BDMethod"),
                 function (x, i, value) {
                     x@methods[[i]] <- value
                     x
                 })

#' @rdname BDMethod-setter
#' @exportMethod "BDMethod<-"
setReplaceMethod("BDMethod",
                 signature(x = "BenchDesign", i = "character", value = "NULL"),
                 function (x, i, value) {
                     x@methods[[i]] <- value
                     x
                 })


#' @rdname BDMethodList-setter
#' @exportMethod "BDMethodList<-"
setReplaceMethod("BDMethodList",
                 signature(x = "BenchDesign", value = "BDMethodList"),
                 function (x, value) {
                     x@methods <- value
                     x
                 })


#' @rdname BDMethod
#' @exportMethod "BDMethod"
setMethod("BDMethod",
          signature(x = "BenchDesign"),
          function(x, i = 1) {
              BDMethod(x@methods, i = i)
          })

#' @rdname BDMethodList
#' @exportMethod "BDMethodList"
setMethod("BDMethodList",
          signature(x = "BenchDesign"),
          function(..., x) {
              x@methods
          })

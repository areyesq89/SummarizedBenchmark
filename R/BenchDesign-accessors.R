#' @rdname BenchDesign-accessors
#' @exportMethod "BDData<-"
setReplaceMethod("BDData",
                 signature(x = "BenchDesign", value = "BDDataOrNULL"),
                 function (x, value) {
                     x@data <- value
                     x
                 })


#' @rdname BenchDesign-accessors
#' @exportMethod "BDMethod<-"
setReplaceMethod("BDMethod",
                 signature(x = "BenchDesign", i = "character", value = "BDMethod"),
                 function (x, i, value) {
                     x@methods[[i]] <- value
                     x
                 })

#' @rdname BenchDesign-accessors
#' @exportMethod "BDMethod<-"
setReplaceMethod("BDMethod",
                 signature(x = "BenchDesign", i = "character", value = "NULL"),
                 function (x, i, value) {
                     x@methods[[i]] <- value
                     x
                 })


#' @rdname BenchDesign-accessors
#' @exportMethod "BDMethodList<-"
setReplaceMethod("BDMethodList",
                 signature(x = "BenchDesign", value = "BDMethodList"),
                 function (x, value) {
                     x@methods <- value
                     x
                 })


#' @rdname BenchDesign-accessors
#' @exportMethod "BDMethod"
setMethod("BDMethod",
          signature(x = "BenchDesign"),
          function(x, i = 1) {
              BDMethod(x@methods, i = i)
          })


#' @rdname BenchDesign-accessors
#' @exportMethod "BDMethodList"
setMethod("BDMethodList",
          signature(x = "BenchDesign"),
          function(..., x) {
              x@methods
          })

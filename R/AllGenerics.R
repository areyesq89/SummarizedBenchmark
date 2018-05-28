#' @rdname BenchDesign-accessors
#' @export
setGeneric("BDData<-", 
           function(x, value) standardGeneric("BDData<-"))

#' @rdname BenchDesign-accessors
#' @export
setGeneric("BDMethod<-", 
           function(x, i, value) standardGeneric("BDMethod<-"))

#' @rdname BenchDesign-accessors
#' @export
setGeneric("BDMethodList<-", 
           function(x, value) standardGeneric("BDMethodList<-"))

#' @rdname BDData
#' @export
setGeneric("BDData", valueClass = "BDDataOrNULL",
           function(data) standardGeneric("BDData"))

#' @rdname hashBDData
#' @export
setGeneric("hashBDData", 
           function(object) standardGeneric("hashBDData"))

#' @rdname BDMethod
#' @export
setGeneric("BDMethod", valueClass = "BDMethod",
           function(x, params = rlang::quos(), post = NULL, meta = NULL, ...) standardGeneric("BDMethod"))

#' @rdname BDMethodList
#' @export
setGeneric("BDMethodList", valueClass = "BDMethodList",
           function(..., x = NULL) standardGeneric("BDMethodList"))

#' @rdname BenchDesign
#' @export
setGeneric("BenchDesign",
           valueClass = "BenchDesign",
           function(..., methods = NULL, data = NULL) standardGeneric("BenchDesign"))

#' @rdname tidyBDMethod
#' @export
setGeneric("tidyBDMethod",
           function(obj, dat = NULL, label = FALSE) standardGeneric("tidyBDMethod"))

#' @rdname compareBenchDesigns
#' @export
setGeneric("compareBenchDesigns",
           function(x, y = NULL, ...) standardGeneric("compareBenchDesigns"))

#' @rdname performanceMetrics
#' @export
setGeneric("performanceMetrics", function( object, ... ) standardGeneric("performanceMetrics"))

#' @rdname performanceMetrics
#' @export
setGeneric( "performanceMetrics<-",
  function( object, ..., value ) standardGeneric( "performanceMetrics<-" ) )

#' @rdname SummarizedBenchmark-accessors
#' @export
setGeneric("groundTruths",
           function( object, ... )
             standardGeneric("groundTruths"))

#' @rdname SummarizedBenchmark-accessors
#' @export
setGeneric( "groundTruths<-",
            function( object, ..., value ) standardGeneric( "groundTruths<-" ) )

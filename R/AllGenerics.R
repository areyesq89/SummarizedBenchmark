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
           function(obj, dat = NULL, eval = FALSE, label = FALSE) standardGeneric("tidyBDMethod"))

#' @rdname compareBenchDesigns
#' @export
setGeneric("compareBenchDesigns",
           function(x, y = NULL, ...) standardGeneric("compareBenchDesigns"))

#' BDData slot setter
#'
#' Method to set BDData slot in objects.
#'  
#' @param x BenchDesign object.
#'
#' @return
#' modified BenchDesign object
#'
#' @name BDData-setter
#' @author Patrick Kimes
#' @export
setGeneric("BDData<-", 
           function(x, value) standardGeneric("BDData<-"))

#' BDMethod slot setter
#'
#' Method to set BDMethod slot in objects.
#'  
#' @param x BenchDesign object.
#'
#' @return
#' modified BenchDesign object
#'
#' @name BDMethod-setter
#' @author Patrick Kimes
#' @export
setGeneric("BDMethod<-", 
           function(x, i, value) standardGeneric("BDMethod<-"))

#' BDMethodList slot setter
#'
#' Method to set BDMethodList slot in objects.
#'  
#' @param x BenchDesign object.
#'
#' @return
#' modified BenchDesign object
#'
#' @name BDMethodList-setter
#' @author Patrick Kimes
#' @export
setGeneric("BDMethodList<-", 
           function(x, value) standardGeneric("BDMethodList<-"))

#' groundTruths setter
#'
#' Method to set groundTruths in SummarizedBenchmark object.
#'  
#' @param object a \code{SummarizedBenchmark} object.
#'
#' @return
#' modified BenchDesign object
#'
#' @name groundTruths-setter
#' @author Patrick Kimes
#' @export
setGeneric("groundTruths<-",
            function( object, ..., value ) standardGeneric( "groundTruths<-" ) )

#' groundTruths getter
#'
#' Method to get groundTruths in SummarizedBenchmark object.
#'  
#' @param object a \code{SummarizedBenchmark} object.
#'
#' @return
#' modified BenchDesign object
#'
#' @name groundTruths
#' @author Patrick Kimes
#' @export
setGeneric("groundTruths",
           function( object, ... )
             standardGeneric("groundTruths"))

#' performanceMetrics slot getter
#'
#' @param object a \code{SummarizedBenchmark} object.
#' @param assay A character string indicating an assay name
#' @param value A SimpleList of the same length as the number of assays
#' @param ... Futher arguments, perhaps used by methods
#'
#' @return
#' A SimpleList with one element for each assay. Each element of the list contains a list of performance metric functions.
#'
#' @examples
#'
#' data( sb )
#' performanceMetrics( sb )
#' performanceMetrics( sb, assay="qvalue" )
#' performanceMetrics( sb ) <- SimpleList( qvalue=list(), logFC=list() )
#'
#' @seealso \code{\link{addPerformanceMetric}}, \code{\link{estimatePerformanceMetrics}}
#' @name performanceMetrics
#' @export
#' @author Alejandro Reyes
setGeneric("performanceMetrics", function( object, ... ) standardGeneric("performanceMetrics"))

#' performanceMetrics slot setter
#'
#' @param object a \code{SummarizedBenchmark} object.
#' @param assay A character string indicating an assay name
#' @param value A SimpleList of the same length as the number of assays
#' @param ... Futher arguments, perhaps used by methods
#'
#' @return
#' A SimpleList with one element for each assay. Each element of the list contains a list of performance metric functions.
#'
#' @examples
#'
#' data( sb )
#' performanceMetrics( sb )
#' performanceMetrics( sb, assay="qvalue" )
#' performanceMetrics( sb ) <- SimpleList( qvalue=list(), logFC=list() )
#'
#' @seealso \code{\link{addPerformanceMetric}}, \code{\link{estimatePerformanceMetrics}}, \code{\link{performanceMetrics}}
#' @name performanceMetrics-setter
#' @export
#' @author Alejandro Reyes
setGeneric("performanceMetrics<-",
  function( object, ..., value ) standardGeneric( "performanceMetrics<-" ) )



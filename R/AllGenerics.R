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

#' Set data in BenchDesign object
#'
#' Adds, removes or replaces \code{\link[=BDData-class]{BDData}} in
#' \code{\link[=BenchDesign-class]{BenchDesign}} object.
#' Data can be removed by setting the value to \code{NULL}.
#' 
#' @param x \code{\link[=BenchDesign-class]{BenchDesign}} object.
#' @param value \code{\link[=BDData-class]{BDData}} or \code{NULL}.
#' 
#' @return
#' modified BenchDesign object
#'
#' @seealso \code{\link{BDData}}
#' @rdname BDData-setter
#' @author Patrick Kimes
#' @export
setGeneric("BDData<-", 
           function(x, value) standardGeneric("BDData<-"))

#' Set method in list or BenchDesign object
#'
#' @description
#' Adds, replaces or removes a named \code{\link[=BDMethod-class]{BDMethod}} method
#' in a \code{\link[=BDMethodList-class]{BDMethodList}} or
#' \code{\link[=BenchDesign-class]{BenchDesign}} object with a specified
#' \code{\link[=BDMethod-class]{BDMethod}} object.
#' 
#' An existing method can be removed by setting the value to \code{NULL}.
#'  
#' @param x \code{\link[=BenchDesign-class]{BenchDesign}} or
#'        \code{\link[=BDMethodList-class]{BDMethodList}} object.
#' @param i character name of method.
#' @param value \code{\link[=BDMethod-class]{BDMethod}} or \code{NULL}.
#'
#' @return
#' modified BenchDesign object
#'
#' @seealso \code{\link{BDMethod}}
#' @rdname BDMethod-setter
#' @author Patrick Kimes
#' @export
setGeneric("BDMethod<-", 
           function(x, i, value) standardGeneric("BDMethod<-"))

#' Set method list in BenchDesign object
#'
#' @description
#' Replaces the \code{\link[=BDMethodList-class]{BDMethodList}} list of methods
#' in a \code{\link[=BenchDesign-class]{BenchDesign}} object with a specified
#' \code{\link[=BDMethodList-class]{BDMethodList}} object.
#' 
#' @param x BenchDesign object.
#' @param value \code{\link[=BDMethodList-class]{BDMethodList}} list of methods.
#' 
#' @return
#' modified BenchDesign object
#'
#' @seealso \code{\link{BDMethod}}, \code{\link{BDMethodList}}
#' @rdname BDMethodList-setter
#' @author Patrick Kimes
#' @export
setGeneric("BDMethodList<-", 
           function(x, value) standardGeneric("BDMethodList<-"))

#' Set ground truths in SummarizedBenchmark object
#'
#' Method to set groundTruths in SummarizedBenchmark object.
#'  
#' @param object a \code{SummarizedBenchmark} object.
#' @param value replacement set of ground truths.
#' @param ... futher arguments, perhaps used by methods.
#' 
#' @return
#' modified BenchDesign object
#'
#' @rdname groundTruths-setter
#' @author Alejandro Reyes, Patrick Kimes
#' @export
setGeneric("groundTruths<-",
            function( object, ..., value ) standardGeneric( "groundTruths<-" ) )

#' Get ground truths in SummarizedBenchmark object
#'
#' Method to get groundTruths in SummarizedBenchmark object.
#'  
#' @param object a \code{SummarizedBenchmark} object.
#' @param ... further arguments, perhaps used by methods.
#'
#' @return
#' modified BenchDesign object
#'
#' @name groundTruths
#' @author Alejandro Reyes
#' @export
setGeneric("groundTruths",
           function( object, ... )
             standardGeneric("groundTruths"))

#' Get performance metrics in SummarizedBenchmark object
#'
#' @description
#' Given a \code{\link[=SummarizedBenchmark-class]{SummarizedBenchmark}} object,
#' returns a list of lists of performance metrics that have been defined for
#' each assay. Optionally, if \code{assay =} is specified, performance metrics
#' for only the specified subset of specified assays are returned.
#' 
#' @param object a \code{\link[=SummarizedBenchmark-class]{SummarizedBenchmark}} object.
#' @param assay a character string indicating an assay name.
#' @param ... futher arguments, perhaps used by methods.
#'
#' @return
#' A SimpleList with one element for each assay. Each element of the list contains a list of performance metric functions.
#'
#' @examples
#' data(sb)
#' performanceMetrics(sb)
#' performanceMetrics(sb, assay = "qvalue")
#'
#' @seealso \code{\link{addPerformanceMetric}}, \code{\link{estimatePerformanceMetrics}}
#' @name performanceMetrics
#' @export
#' @author Alejandro Reyes
setGeneric("performanceMetrics", function( object, ... ) standardGeneric("performanceMetrics"))

#' Set performance metrics in SummarizedBenchmark object
#'
#' @description
#' Replaces the list of performance metrics in a
#' \code{\link[=SummarizedBenchmark-class]{SummarizedBenchmark}} object with
#' a new list of performance metric lists. 
#' 
#' @param object a \code{\link[=SummarizedBenchmark-class]{SummarizedBenchmark}} object.
#' @param value a SimpleList of the same length as the number of assays.
#' @param ... futher arguments, perhaps used by methods.
#'
#' @return
#' Silently, the newly specified SimpleList of performance metric lists.
#'
#' @examples
#' data(sb)
#' performanceMetrics(sb)
#' performanceMetrics(sb) <- SimpleList(qvalue = list(), logFC = list())
#'
#' @seealso \code{\link{addPerformanceMetric}}, \code{\link{estimatePerformanceMetrics}}, \code{\link{performanceMetrics}}
#' @rdname performanceMetrics-setter
#' @export
#' @author Alejandro Reyes
setGeneric("performanceMetrics<-",
  function( object, ..., value ) standardGeneric( "performanceMetrics<-" ) )



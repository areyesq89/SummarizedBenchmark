#' BenchDesign Accessors
#'
#' These methods can be used to access, set, and remove elements of
#' a BenchDesign object.
#'  
#' @param x BenchDesign object.
#' @param i character name or integer index of a BDMethod object.
#'        (Note: must be a character name for replacement operations.)
#' @param value replacement object, either BDData, BDMethod, BDMethodList
#'        or NULL object.
#'
#' @return
#' modified BenchDesign object
#'
#' @name BenchDesign-accessors
#' @author Patrick Kimes
NULL

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

#' Accessor and replacement functions for the slot 'performanceMetrics' of a SummarizedBenchmark object.
#'
#' @docType methods
#' @name performanceMetrics
#' @rdname performanceMetrics
#' @aliases performanceMetrics performanceMetrics,SummarizedBenchmark-method performanceMetrics<-,SummarizedBenchmark,SimpleList-method
#'
#' @param object a \code{SummarizedBenchmark} object.
#' @param assay A character string indicating an assay name
#' @param value A SimpleList of the same length as the number of assays
#' @param ... Futher arguments, perhaps used by methods
#' @seealso \code{\link{addPerformanceMetric}}, \code{\link{estimatePerformanceMetrics}}
#'
#' @examples
#'
#' data( sb )
#' performanceMetrics( sb )
#' performanceMetrics( sb, assay="qvalue" )
#' performanceMetrics( sb ) <- SimpleList( qvalue=list(), logFC=list() )
#'
#' @author Alejandro Reyes
#'
#' @return A SimpleList with one element for each assay. Each element of the list contains a list of performance functions.
NULL

#' Accessor and replacement functions for the slots of a SummarizedBenchmark object.
#' @docType methods
#' @name SummarizedBenchmark-accessors
#' @rdname SummarizedBenchmark-accessors
#'
#' @param x a \code{SummarizedBenchmark} object.
#' @param value A character vector
#' @param ... Futher arguments, perhaps used by methods
#' @seealso \code{\link{performanceMetrics}}
#'
#' @return Either a \code{SummarizedBenchmark} object or a slot of the \code{SummarizedBenchmark} object.
#'
#' @examples
#'
#' data( sb )
#' assayNames( sb )[2] <- "log2FC"
#'
#' @author Alejandro Reyes
#' @importMethodsFrom SummarizedExperiment assayNames `assayNames<-`
NULL


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
           function(obj, dat = NULL, eval = FALSE, label = FALSE) standardGeneric("tidyBDMethod"))

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

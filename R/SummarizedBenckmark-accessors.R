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

performanceMetricsSB <- function( object, assay=NULL ){
  validObject( object )
  if( is.null( assay ) ){
    return( object@performanceMetrics )
  }else{
    inAssay <- assay %in% assayNames( object )
    if( !all( inAssay ) ){
      stop( sprintf( "Performance metric(s) '%s' not found.", assay[!inAssay] ) )
    }
    return( object@performanceMetrics[assay] )
  }
}

#' @rdname performanceMetrics
#' @export
setMethod( "performanceMetrics",
  signature( object = "SummarizedBenchmark" ), performanceMetricsSB )


#' @name performanceMetrics
#' @rdname performanceMetrics
#' @exportMethod "performanceMetrics<-"
setReplaceMethod( "performanceMetrics",
                  signature(object="SummarizedBenchmark", value="SimpleList"),
                 function( object, value ) {
                   object@performanceMetrics <- value
                   validObject(object)
                   object
                 } )



#' Accessor and replacement functions for the slots of a SummarizedBenchmark object.
#' @docType methods
#' @name SummarizedBenchmark-accessors
#' @rdname SummarizedBenchmark-accessors
#' @aliases assayNames assayNames,SummarizedBenchmark-method assayNames<-,SummarizedBenchmark,character-method
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
#' @importMethodsFrom SummarizedExperiment assayNames
#' @export
setReplaceMethod( "assayNames", c("SummarizedBenchmark", "character"),
                  function(x, ..., value)
{
  oldNames <- names( assays( x, withDimnames=FALSE ) )
  names( assays( x, withDimnames=FALSE ) ) <- value
  newNames <- names( assays( x, withDimnames=FALSE ) )
  mm <- match( names( x@performanceMetrics ),  oldNames )
  names( x@performanceMetrics )[mm] <- newNames
  truthCol <- elementMetadata( rowData( x ) )$colType == "groundTruth"
  truthCol[is.na(truthCol)] <- FALSE
  if( any( truthCol ) ){
    mm <- match( colnames( rowData( x ) )[truthCol], oldNames )
    colnames(rowData( x ))[truthCol][mm] <- newNames
  }
  x
} )

#' @rdname SummarizedBenchmark-accessors
#' @aliases mcols<-,SummarizedBenchmark-method
#' @import BiocGenerics
#' @export
setReplaceMethod("mcols", "SummarizedBenchmark",
    function(x, ..., value)
{
    x <- BiocGenerics:::replaceSlots(x,
        rowRanges=local({
            r <- rowRanges( x )
            if( length( value ) > 0 ){
              wc <- colnames( value ) %in% assayNames( x )
              mcols( value )$colType <- ifelse( wc, "groundTruth", "featureData" )
            }
            mcols( r ) <- value
            r
        }),
        check=FALSE)
    x
})

#' @rdname SummarizedBenchmark-accessors
#' @aliases groundTruths groundTruths,SummarizedBenchmark-method groundTruths<-,SummarizedBenchmark-method
#' @param object a \code{SummarizedBenchmark} object.
#' @export
setMethod(
  "groundTruths",
  "SummarizedBenchmark",
  function( object, ... ){
    stopifnot( is( object, "SummarizedBenchmark") )
    cols <- mcols( mcols(object) )$colType == "groundTruth"
    mcols(object, ...)[,cols, drop=FALSE]
  }
)

#' @rdname SummarizedBenchmark-accessors
#' @exportMethod "groundTruths<-"
setReplaceMethod(
  "groundTruths",
  "SummarizedBenchmark",
  function(object, ..., value){
    mcols(object, ...) <- value
    object
  }
)

#' @rdname SummarizedBenchmark-accessors
#' @export
setMethod("BDMethodList",
          signature(x = "SummarizedBenchmark"),
          function(..., x) {
              BDMethodList(x = BenchDesign(methods = x))
          })



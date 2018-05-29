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
#' @exportMethod "performanceMetrics"
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


#' @rdname SummarizedBenchmark-accessors
#' @aliases assayNames assayNames,SummarizedBenchmark-method assayNames<-,SummarizedBenchmark,character-method
#' @exportMethod "assayNames<-"
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
#' @importMethodsFrom S4Vectors mcols elementMetadata `mcols<-` `elementMetadata<-`
#' @exportMethod "mcols<-"
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
#' @exportMethod "groundTruths"
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
#' @exportMethod "BDMethodList"
setMethod("BDMethodList",
          signature(x = "SummarizedBenchmark"),
          function(..., x) {
              BenchDesign(x)@methods
          })


#' @rdname SummarizedBenchmark-accessors
#' @exportMethod "BenchDesign"
setMethod("BenchDesign", signature(methods = "SummarizedBenchmark", data = "ANY"),
          function(methods, data) {
              if (!is.null(data))
                  message("Note: Ignoring specified data and using SummarizedBenchmark object.")
              bd <- methods@BenchDesign
              if (is.null(bd))
                  bd <- BenchDesign()
              bd
          })

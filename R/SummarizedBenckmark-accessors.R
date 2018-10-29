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
setMethod("performanceMetrics",
          signature(object = "SummarizedBenchmark"), performanceMetricsSB)

#' @rdname performanceMetrics-setter
#' @exportMethod "performanceMetrics<-"
setReplaceMethod("performanceMetrics",
                 signature(object="SummarizedBenchmark", value="SimpleList"),
                 function( object, value ) {
                     object@performanceMetrics <- value
                     validObject(object)
                     object
                 })

#' Set assay names in SummarizedBenchmark object
#'
#' Modifies the assay names of a \code{\link[=SummarizedBenchmark-class]{SummarizedBenchmark}}
#' object.
#' 
#' @param x A \code{\link[=SummarizedBenchmark-class]{SummarizedBenchmark}} object.
#' @param value A character vector.
#' @param ... Futher arguments, perhaps used by methods.
#'
#' @return
#' Modfied \code{\link[=SummarizedBenchmark-class]{SummarizedBenchmark}} object.
#'
#' @examples
#' data(sb)
#' assayNames(sb)[2] <- "log2FC"
#'
#' @seealso \code{\link[=SummarizedBenchmark-class]{SummarizedBenchmark}}
#' @name assayNames-setter
#' @importMethodsFrom SummarizedExperiment assayNames "assayNames<-"
#' @exportMethod "assayNames<-"
#' @author Alejandro Reyes
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


#' Set meta data columns in SummarizedBenchmark object
#'
#' Modifies the \code{mcols} slot of a \code{\link[=SummarizedBenchmark-class]{SummarizedBenchmark}}
#' object.
#' 
#' @param x A \code{\link[=SummarizedBenchmark-class]{SummarizedBenchmark}} object.
#' @param value A DataFrame of meta data.
#' @param ... Futher arguments, perhaps used by methods.
#'
#' @return
#' Modfied \code{\link[=SummarizedBenchmark-class]{SummarizedBenchmark}} object.
#'
#' @seealso \code{\link[=SummarizedBenchmark-class]{SummarizedBenchmark}}
#' @name mcols-setter
#' @importMethodsFrom S4Vectors mcols elementMetadata "mcols<-" "elementMetadata<-"
#' @exportMethod "mcols<-"
#' @author Alejandro Reyes
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

#' @rdname groundTruths
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

#' @rdname groundTruths-setter
#' @exportMethod "groundTruths<-"
setReplaceMethod(
  "groundTruths",
  "SummarizedBenchmark",
  function(object, ..., value){
    mcols(object, ...) <- value
    object
  }
)

#' @rdname BDMethodList
#' @exportMethod "BDMethodList"
setMethod("BDMethodList",
          signature(x = "SummarizedBenchmark"),
          function(..., x) {
              BenchDesign(x)@methods
          })


#' @rdname BenchDesign
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

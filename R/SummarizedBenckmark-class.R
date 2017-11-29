#' @name SummarizedBenchmark-class
#' @title SummarizedBenchmark class documentation
#' @description
#' Extension of the \code{\link{RangedSummarizedExperiment}} to
#' store the output of different methods intended for the same purpose
#' in a given dataset. For example, a differential expression analysis could be
#' done using \pkg{limma}-voom, \pkg{edgeR} and \pkg{DESeq2}. The
#' SummarizedBenchmark class provides a framework that is useful to store, benckmark and
#' compare results.
#' @slot performanceMetrics A \code{\link{SimpleList}} of the same length
#' as the number of \code{\link{assays}} containing performance
#' functions to be compared with the ground truths.
#'
#' @author Alejandro Reyes
#'
#' @aliases SummarizedBenchmark-class
#' @importClassesFrom SummarizedExperiment RangedSummarizedExperiment
#' @importFrom methods as formalArgs is new validObject
#' @import S4Vectors SummarizedExperiment
#' @export
#' @exportClass SummarizedBenchmark
setClass( "SummarizedBenchmark",
         contains = "RangedSummarizedExperiment",
         representation = representation( performanceMetrics = "SimpleList" ) )

setValidity( "SummarizedBenchmark", function( object ) {
  if( ! length( object@performanceMetrics ) == length( assays( object ) ) ){
    stop("The number of elements of the slot 'performanceFunction' has
         be of the same length as the number of assays.")
  }
#  if( ncol( rowData( object ) ) > 0 & !all( assayNames( object ) %in% colnames( rowData( object ) ) ) ){
#    stop("Not all assays have a corresponding ground truth column in rowData")
#  }
  if( !all( names( object@performanceMetrics ) %in% assayNames( object  ) ) ){
    stop("The names of the performanceMetrics list must match the names of the assays")
  }
  if( !all( sapply( object@performanceMetrics, "class" ) == "list" ) ){
    stop("In the slot 'performanceMetrics', each element of the list must contain a list of functions")
  }
  permFunc <- object@performanceMetrics
  for( i in names( permFunc ) ){
    funcAssay <- permFunc[[i]]
    for(j in names(funcAssay) ){
      y <- funcAssay[[j]]
      f1 <- all( c("query", "truth") %in% formalArgs( y ) )
      if( !f1 ){
        stop(sprintf( "The performance function '%s' for assay '%s' does not contain a 'query' and/or a 'truth' argument", j, i) )
      }
      otherArgs <- formals(y)[!names(formals(y)) %in% c("query", "truth")]
      f2 <- any( sapply(otherArgs, class) == "name" )
      if( f2 ){
        missingDefaults <- names(otherArgs)[which(f2)]
        stop(sprintf("The parameter(s) '%s' of the performance function '%s' for assay '%s' has/have no default values. Please specify defaults values.", paste(missingDefaults, collapse=","), j, i ) )
      }
    }
  }
  TRUE
} )


#' @title Constructor function for SummarizedBenchmark objects.
#' @aliases SummarizedBenchmark
#' @description
#' Function to construct \code{SummarizedBenchmark} objects.
#'
#' @param assays A list containing outputs of the methods to be
#' benchmark. Each element of the list must contain a matrix or
#' data.frame of n x m, n being the number of features tested
#' (e.g. genes) and m being the number of methods in the benchmark.
#' Each element of the list must contain a single assay
#' (the outputs of the methods). For example,
#' for a benchmark of differential expression methods, one assay could contain
#' the q-values from the different methods and another assay could be the
#' estimated log fold changes.
#' @param colData A \code{\link{DataFrame}} describing the annotation of the
#' methods. These could include version of the software or the parameters
#' used to run them.
#' @param ftData A \code{\link{DataFrame}} object describing the rows. This parameter
#' is equivalent to the parameter rowData of a SummarizedExperiment.
#' @param groundTruth If present, a \code{\link{DataFrame}} containing the ground truths.
#' If provided, the number of columns must be the same as the number of assays
#' (NA's are accepted).
#' The names of the columns should have the same names as the assays.
#' @param performanceMetrics A \code{\link{SimpleList}} of the same
#' length as the number of assays. Each element of the list must be a list
#' of functions. Each function must contain the parameters 'query' and
#' 'truth'.
#' @param ... Additional parameters passed to \code{\link{SummarizedExperiment}}.
#'
#' @author Alejandro Reyes
#'
#' @examples
#'
#' ## loading the example data from iCOBRA
#' library(iCOBRA)
#' data(cobradata_example)
#'
#' ## a bit of data wrangling and reformatting
#' assays <- list(
#'     qvalue=cobradata_example@padj,
#'     logFC=cobradata_example@score )
#' assays[["qvalue"]]$DESeq2 <- p.adjust(cobradata_example@pval$DESeq2, method="BH")
#' groundTruth <- DataFrame( cobradata_example@truth[,c("status", "logFC")] )
#' colnames(groundTruth) <- names( assays )
#' colData <- DataFrame( method=colnames(assays[[1]]) )
#' groundTruth <- groundTruth[rownames(assays[[1]]),]
#'
#' ## constructing a SummarizedBenchmark object
#' sb <- SummarizedBenchmark(
#'     assays=assays, colData=colData,
#'     groundTruth=groundTruth )
#'
#' @return A \code{\link{SummarizedBenchmark}} object.
#' @importFrom tidyr gather
#' @importFrom S4Vectors DataFrame
#' @export
#'
SummarizedBenchmark <- function( assays, colData, ftData=NULL,
                                 groundTruth=NULL, performanceMetrics=NULL, ... ){
  if( is( colData, "data.frame" ) ){
    colData <- DataFrame( colData )
  }
  if( is( ftData, "data.frame" ) ){
    ftData <- DataFrame( ftData )
  }
  if( is( groundTruth, "data.frame" ) ){
    groundTruth <- DataFrame( groundTruth )
  }
  if(is.null( groundTruth )){
    groundTruth <- matrix(NA, ncol=length(assays), nrow=nrow(assays[[1]]))
    colnames( groundTruth ) <- names(assays)
    rownames( groundTruth ) <- rownames( assays[[1]] )
    groundTruth <- DataFrame( groundTruth )
  }
  if( length( unique( sapply( assays, nrow ) ) ) > 1 ){
    stop("All metric matrices must have the same number of rows.")
  }
  elementMetadata(groundTruth) <- DataFrame(colType="groundTruth")
  if( is.null( ftData ) ){
    rData <- groundTruth
  }else{
    elementMetadata( ftData ) <- DataFrame( colType="featureData" )
    rData <- cbind( groundTruth, ftData )
  }
  if( length( unique( sapply( assays, ncol ) ) ) > 1 ){
    stop("All assays must contain the same number of columns")
  }
  if( is.null(performanceMetrics)){
    performanceMetrics <- vector("list", 2 )
    performanceMetrics <- as( performanceMetrics, "SimpleList" )
    names( performanceMetrics ) <- names(assays)
    for( i in seq_along( performanceMetrics ) ){
      performanceMetrics[[i]] <- list()
    }
  }
  elementMetadata( colData ) <- DataFrame( colType="methodInformation" )
  se <- as( SummarizedExperiment( assays, colData=colData, ... ),
            "RangedSummarizedExperiment")
  rowData(se) <- rData
  sb <- new( "SummarizedBenchmark", se, performanceMetrics=performanceMetrics )
  sb
}

#' @rdname performanceMetrics
#' @export
setGeneric("performanceMetrics", function( object, ... ) standardGeneric("performanceMetrics"))

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
#' @export
setMethod( "performanceMetrics",
  signature( object = "SummarizedBenchmark" ), performanceMetricsSB )

#' @rdname performanceMetrics
#' @export
setGeneric( "performanceMetrics<-",
  function( object, ..., value ) standardGeneric( "performanceMetrics<-" ) )

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
#' @name Accessors
#' @rdname Accessors
#' @aliases assayNames assayNames,SummarizedBenchmark-method assayNames<-,SummarizedBenchmark,character-method
#'
#' @param x a \code{SummarizedBenchmark} object.
#' @param value A character vector
#' @param ... Futher arguments, perhaps used by methods
#' @seealso \code{\link{performanceMetrics}}
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

#' @rdname Accessors
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

#' @rdname Accessors
#' @export
setGeneric("groundTruths",
           function( object, ... )
             standardGeneric("groundTruths"))

#' @rdname Accessors
#' @aliases groundTruths groundTruths,SummarizedBenchmark-method groundTruths<-,SummarizedBenchmark-method
#' @param object a \code{SummarizedBenchmark} object.
#' @export
setMethod(
  "groundTruths",
  "SummarizedBenchmark",
  function( object, ... ){
    stopifnot( is( object, "SummarizedBenchmark") )
    cols <- mcols( mcols(object) )$colType == "groundTruth"
    mcols(object, ...)[,cols]
  }
)

#' @rdname Accessors
#' @export
setGeneric( "groundTruths<-",
            function( object, ..., value ) standardGeneric( "groundTruths<-" ) )

#' @rdname Accessors
#' @exportMethod "groundTruths<-"
setReplaceMethod(
  "groundTruths",
  "SummarizedBenchmark",
  function(object, ..., value){
    mcols(object, ...) <- value
    object
  }
)


#' SummarizedBenchmark example
#'
#' @name sb
#' @docType data
#' @keywords data SummarizedBenchmark
NULL

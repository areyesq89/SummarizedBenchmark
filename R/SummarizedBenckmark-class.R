#' @describeIn SummarizedBenchmark
#' @name SummarizedBenchmark-class
#' @title SummarizedBenchmark class documentation
#' @description Extension of the \code{\link{RangedSummarizedExperiment}} to
#' store the output of different methods intended for the same purpose
#' in a given dataset. For example, a differential expression analysis could be
#' done using \pkg{limma}-voom, \pkg{edgeR} and \pkg{DESeq2}. The
#' SummarizedBenchmark class provides a framework that is useful to store
#' and compare the results from the three different programs.
#' @slot performanceFunctions A \code{\link{SimpleList}} of the same length
#' as the number of \code{\link{assays}} containing performance
#' functions to be compared with the ground truths.
#' @aliases SummarizedBenchmark-class
#' @importClassesFrom SummarizedExperiment RangedSummarizedExperiment
#' @export
#' @exportClass SummarizedBenchmark
setClass( "SummarizedBenchmark",
         contains = "RangedSummarizedExperiment",
         representation = representation( performanceFunctions = "SimpleList" ) )

setValidity( "SummarizedBenchmark", function( object ) {
  if( ! length( object@performanceFunctions ) == length( assays( object ) ) ){
    stop("The number of elements of the slot 'performanceFunction' has
         be of the same length as the number of assays.")
  }
  if( !all( names( assays( object ) ) %in% colnames( rowData( object ) ) ) ){
    stop("Not all assays have a corresponding ground truth column in rowData")
  }
  if( !all( names( object@performanceFunctions ) %in% names( assays( object ) ) ) ){
    stop("The names of the performanceFunctions list must match the names of the assays")
  }
  if( !all( sapply( object@performanceFunctions, "class" ) == "list" ) ){
    stop("Each list element of of the performanceFunctions slot must be a list of functions")
  }
  for( i in seq_len( length( object@performanceFunctions ) ) ){
    goodArgs <- sapply( object@performanceFunctions, function(x){
      if( length( x ) > 0 ){
        sapply( x, function(y){
          all( c("query", "truth") %in% formalArgs( y ))
        } )
      }
    } )
  }
  if( !is.null( unlist( goodArgs ) ) & !all( unlist( goodArgs ) ) ){
    stop("All performance functions must contain a `query` and a `truth` argument")
  }
  TRUE
} )


SummarizedBenchmark <- function( assays, colData, ftData=NULL, groundTruth=NULL,
                                 performanceFunctions=NULL, ... ){
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
  if( is.null( ftData ) ){
    rData <- DataFrame(groundTruth)
    elementMetadata(rData) <- DataFrame(colType="groundTruth")
  }else{
    elementMetadata( ftData ) <- DataFrame( colType="featureData" )
    rData <- cbind( groundTruth, ftData )
  }
  if( length( unique( sapply( assays, ncol ) ) ) > 1 ){
    stop("All assays must contain the same number of columns")
  }
  if( is.null(performanceFunctions)){
    performanceFunctions <- vector("list", 2 )
    performanceFunctions <- as( performanceFunctions, "SimpleList" )
    names( performanceFunctions ) <- names(assays)
    for( i in seq_along( performanceFunctions ) ){
      performanceFunctions[[i]] <- list()
    }
  }
  elementMetadata( colData ) <- DataFrame( colType="methodInformation" )
  se <- as( SummarizedExperiment( assays, colData=colData, ... ),
          "RangedSummarizedExperiment")
  rowData(se) <- rData
  sb <- new( "SummarizedBenchmark", se, performanceFunctions=performanceFunctions )
  sb
}

addPerformanceFunction <- function( object, evalMetric, assay, FUN ){
  if( !all( c("query", "truth") %in% formalArgs( FUN ) ) ){
    stop("All performance functions need to have both a `query` and a `truth` argument")
  }
  if( is.null( evalMetric ) ){
    stop("Please specify a name for the new evaluation metric (parameter evalMetric).")
  }
  if( is.null( assay ) ){
    stop("Please specify an assay for the new evaluation metric (parameter assay).")
  }
  object@performanceFunctions[[assay]][[evalMetric]] <- FUN
  validObject( object )
  object
}

estimateMetricsForAssay <- function( object, assay, evalMetric=NULL, addColData=FALSE, evalFunction=NULL, ...){
  stopifnot( is( object, "SummarizedBenchmark" ) )
  stopifnot( all( assay %in% names( assays( object ) ) ) )
  if( !is.null(evalFunction) ){
    object <- addPerformanceFunction( object, evalMetric, assay, evalFunction )

  }
  allFunctions <- object@performanceFunctions[[assay]]
  if( length(allFunctions) == 0 ){
    stop(sprintf( "Metric functions not specified for assay(s): %s. Check `?addPerformanceFunction`.", assay ) )
  }
  if( !is.null( evalMetric ) ){
    if( !all(evalMetric %in% names(allFunctions)) ){
      notDefined <- evalMetric[!evalMetric %in% names(allFunctions)]
      stop(sprintf("Function could not be found for metric(s) %s", paste(notDefined, collapse=",")))
    }
    allFunctions <- allFunctions[names( allFunctions ) %in% evalMetric]
  }
  allDotArgs <- as.list( match.call( expand.dots=FALSE ) )[["..."]]
  assayData <- assays(object)[[assay]]
  assayTruth <- rowData(object)[,assay]
  res <- lapply( names( allFunctions ), function( nf ){
    f <- allFunctions[[nf]]
    vecArgs <- formalArgs( f )[ !formalArgs( f ) %in% c("query", "truth") ]
    passArgs <- list(truth=assayTruth)
    eleMD <- DataFrame( colType="performanceMetric", assay=assay, performanceMetric=nf )
    if( length( vecArgs ) > 0 ){
      vf <- Vectorize( f, vectorize.args=vecArgs )
      defaultArgs <- formals(f)[vecArgs[!vecArgs %in% names( passArgs )]]
      extraDotArgs <- allDotArgs[names(allDotArgs) %in% formalArgs(f)]
      if( length( extraDotArgs ) > 0 ){
        for( i in names( extraDotArgs ) ){
          defaultArgs[[i]] <- eval( extraDotArgs[[i]] )
        }
      }
      passArgs <- c( passArgs, defaultArgs )
      resNRow <- max( sapply( passArgs[vecArgs], length ) )
      eleMD <- cbind( eleMD, DataFrame( as.data.frame( passArgs[vecArgs] ) ) )
    }else{
      vf <- f
      resNRow <- 1
    }
    indRes <- sapply( seq_len( ncol( assayData ) ), function( i ){
      assayRes <- do.call( vf, c( list(query=assayData[,i]), passArgs ) )
      assayRes
    } )
    resDF <- DataFrame( t( matrix( indRes, nrow=resNRow ) ) )
    elementMetadata( resDF ) <- eleMD
    if( resNRow > 1 ){
      colnames( resDF ) <- paste( nf, seq_len( ncol( resDF ) ), sep=".")
    }else{
      colnames( resDF ) <- nf
    }
    resDF
  } )
  names( res ) <- names( allFunctions )
  res <- Reduce( cbind, res )
  res <- cbind( colData( object ), res )
  if( addColData ){
    colData( object ) <- res
    return( object )
  }else{
    return( res )
  }
}

estimatePerformanceMetrics <- function( object, addColData=FALSE, ... ){
  stopifnot( is( object, "SummarizedBenchmark" ) )
  assayNames <- names( assays( object ) )
  allRes <- lapply( assayNames, function(x){
    if( length( object@performanceFunctions[[x]] ) > 0 ){
      estimateMetricsForAssay( object, assay=x, ... )
    }else{
      NULL
    }
  } )
  allRes <- allRes[!sapply(allRes, is.null)]
  if( length( allRes ) > 0 ){
    allRes <- 
      lapply( allRes, 
        function(x){ 
          x[,elementMetadata( x )$colType == "performanceMetric",drop=FALSE]
        } )
    allRes <- Reduce( cbind, allRes )
  }else{
    stop("Metric functions not found for any assay. Check `?addPerformanceFunction`. to include these." )
  }
  allRes <- cbind( colData(object), allRes )
  if( addColData ){
    colData(object) <- allRes
    return( object )
  }else{
    return( allRes )
  }
}


library(iCOBRA)
data(cobradata_example)

cobradata_example[,1]

assays <- list(
  qval=cobradata_example@padj,
  logFC=cobradata_example@score )
assays[["qval"]]$DESeq2 <- p.adjust(cobradata_example@pval$DESeq2, method="BH")
groundTruth <- DataFrame( cobradata_example@truth[,c("status", "logFC")] )
colnames(groundTruth) <- names( assays )
ftData <- NULL
colData <- DataFrame( method=colnames(assays[[1]]) )
performanceFunctions <- NULL
groundTruth <- groundTruth[rownames(assays[[1]]),]

### replace assay with: result? output? metric?

sb <- SummarizedBenchmark(
  assays=assays, colData=colData,
  groundTruth=groundTruth, ftData=ftData )

estimatePerformanceMetrics(sb)

sb <- addPerformanceFunction(
  object=sb,
  assay="qval",
  evalMetric="TPR",
  FUN = function( query, truth, alpha=0.1 ){
    goodHits <- sum( (query < alpha) & truth == 1 )
    goodHits / sum(truth == 1)
    }
)

sb <- addPerformanceFunction(
  object=sb,
  evalMetric="jaccardIndex",
  assay="qval",
  FUN = function( query, truth, alpha=0.1){
    goodHits <- sum( xor( query < alpha, as.logical( truth ) ) )
    goodHits / length( truth )
  }
)

estimatePerformanceMetrics( sb, alpha=c(0.1, 0.2, 0.3) )

sb <- addPerformanceFunction(
  object=sb,
  evalMetric="EucledianDist",
  assay="logFC",
  FUN <- function( query, truth ){
    sqrt( sum( (query - truth)^2 ) )
  }
)

## add option `tidy=TRUE` for estimateMetrics*

colData( estimateMetricsForAssay( sb, evalMetric="TPR", assay="qval", alpha=c( 0.1, 0.2, 0.3, 0.4 ), addColData=TRUE ) )

estimateMetricsForAssay( sb, evalMetric="TPR", assay="qval", alpha=0.1 )


t( matrix(estimateMetricsForAssay( sb, assay="qval", alpha=0.1)[[1]], nrow=1 ) )

estimateMetricsForAssay( sb, assay="logFC" )

estimateMetricsForAssay(
  sb,
  assay="qvalue",
  evalMetric="rejections",
  evalFunction=function(query, truth, alpha=0.1){
    sum( query < alpha )
  } )


elementMetadata( estimatePerformanceMetrics( sb, alpha=c(0.1, 0.2, 0.3) ) )

estimateMetricsForAssay( sb, assay="logFC" )[,2,drop=FALSE]

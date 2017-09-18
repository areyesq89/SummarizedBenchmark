#' @name SummarizedBenchmark-class
#' @title SummarizedBenchmark class documentation
#' @description
#' Extension of the \code{\link{RangedSummarizedExperiment}} to
#' store the output of different methods intended for the same purpose
#' in a given dataset. For example, a differential expression analysis could be
#' done using \pkg{limma}-voom, \pkg{edgeR} and \pkg{DESeq2}. The
#' SummarizedBenchmark class provides a framework that is useful to store
#' and compare the results from the three different software
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
    stop("In the slot 'performanceFunctions', each element of the list must contain a list of functions")
  }
  permFunc <- object@performanceFunctions
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
#' (the outputs of the methods to be benchmarked). For example,
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
#' @param performanceFunctions A \code{\link{SimpleList}} of the same
#' length as the number of assays. Each element of the list must be a list
#' of functions. Each function must contain the parameters 'query' and
#' 'truth'.
#' @param ... Additional parameters passed to \code{\link{SummarizedExperiment}}.
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
#'     groundTruth=groundTruth, ftData=ftData )
#'
#' @return A \code{\link{SummarizedBenchmark}} object.
#' @importFrom tidyr gather
#' @importFrom S4Vectors DataFrame
#' @export
#'
SummarizedBenchmark <- function( assays, colData, ftData=NULL,
                                 groundTruth=NULL, performanceFunctions=NULL, ... ){
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

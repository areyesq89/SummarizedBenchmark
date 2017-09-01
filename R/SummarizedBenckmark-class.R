#' @describeIn SummarizedBenchmark
#' @name SummarizedBenchmark-class
#' @title SummarizedBenchmark class documentation
#' @description Extension of the \code{\link{RangedSummarizedExperiment}} to
#' store the output of different methods intended for the same purpose
#' in a given dataset. For example, a differential expression analysis could be
#' done using \pkg{limma}-voom, \pkg{edgeR} and \pkg{DESeq2}. The
#' SummarizedBenchmark class provides a framework that is useful to store
#' and compare the results from the three different programs.
#' @slot evaluationFunctions A \code{\link{SimpleList}} of the same length
#' as the number of \code{\link{assays}} containing evaluation
#' functions to be compared with the ground truths.
#' @aliases SummarizedBenchmark-class
#' @importClassesFrom SummarizedExperiment RangedSummarizedExperiment
#' @export
#' @exportClass SummarizedBenchmark
setClass("SummarizedBenchmark",
         slots=c( evaluationFunctions = "SimpleList" ),
         contains = "RangedSummarizedExperiment" )

setValidity( "SummarizedBenchmark", function( object ) {
  if( ! length( object@evaluationFunctions ) == length( assays( object ) ) ){
    stop("The number of elements of the slot 'evaluationFunction' has
         be of the same length as the number of assays.")
  }
  if( names( assays( object ) ) %in% colnames( rowData( object ) ) ){
    stop("Not all assays have a corresponding ground truth column in rowData")
  }
  TRUE
} )

#SummarizedBenchmark <- function( se, evaluationFunctions ){
#
#}

SummarizedBenchmark <- function( assays, rowData, colData, evaluationFunctions=NULL ){

}

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
#' @param BenchDesign A \code{\link{BenchDesign}} containing the code used
#'        to construct the object. (default = NULL)
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
#' colData(sb)$label <- rownames(colData(sb))
#' 
#' @return A \code{\link{SummarizedBenchmark}} object.
#' @importFrom tidyr gather
#' @importFrom S4Vectors DataFrame elementMetadata
#' @export
#'
SummarizedBenchmark <- function(assays, colData, ftData = NULL,
                                groundTruth = NULL, performanceMetrics = NULL,
                                BenchDesign = NULL, ... ) {
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
  sb <- new( "SummarizedBenchmark", se, performanceMetrics=performanceMetrics,
            BenchDesign = BenchDesign)
  sb
}

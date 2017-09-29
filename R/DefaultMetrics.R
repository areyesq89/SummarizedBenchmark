TPR <- function( query, truth, alpha=0.1 ){
  sum( query < alpha & truth == 1 ) / sum( truth == 1 )
}

TNR <- function( query, truth, alpha=0.1 ){
  sum( ( !query < 0.1 ) & truth == 0 ) / sum( truth == 0 )
}

FPR <- function( query, truth, alpha=0.1 ){
  sum( query < 0.1 & truth == 0 ) / sum( query < 0.1 )
}

FNR <- function( query, truth, alpha=0.1 ){
  sum( !(query < 0.1) & truth == 1 ) / sum( !( query < 0.1 ) )
}

rejections <- function( query, truth, alpha=0.1 ){
  sum( query < alpha )
}

#' @title Add default performance metrics to a \code{\link{SummarizedBenchmark}} object.
#' @aliases addDefaultMetrics
#' @description
#' This function adds predefined performance metrics to a \code{\link{SummarizedBenchmark}}
#' object, according to the assay name. For example, if an assay is named "qvalue",
#' it will add a the performance metric "rejections".
#'
#' @param object A \code{\link{SummarizedBenchmark}} object.
#'
#' @examples
#'
#' data( sb )
#' sb <- addDefaultMetrics( sb )
#'
#' @return A \code{\link{SummarizedBenchmark}} object with performance functions.
#' @details This function adds a set of predefined performance metrics according
#' to the names of the assays. Specifically, if an assay is called "qvalue" it will
#' add the "rejections" metric by default. Furthermore, if the qvalue assay has
#' information about the ground truths in the rowData, the metrics TPR (true
#' positive rate), "TNR" (true negative rate), "FPR" (false positive rate) and
#' "FNR" (false negative rate) will be added.
#'
#' @author Alejandro Reyes
#'
#' @export
#'
addDefaultMetrics <- function( object ){
  if( any( assayNames( object ) %in% "qvalue" ) ){
    object <- addPerformanceMetric(
      object=object,
      assay="qvalue",
      evalMetric="rejections",
      evalFunction = rejections )
    if( all(!is.na( rowData( object )[["qvalue"]] )) ){
      object <- addPerformanceMetric(
        object=object,
        assay="qvalue",
        evalMetric="TPR",
        evalFunction = TPR )
      object <- addPerformanceMetric(
        object=object,
        assay="qvalue",
        evalMetric="TNR",
        evalFunction = TNR )
      object <- addPerformanceMetric(
        object=object,
        assay="qvalue",
        evalMetric="FPR",
        evalFunction=FPR )
      object <- addPerformanceMetric(
        object=object,
        assay="qvalue",
        evalMetric="FNR",
        evalFunction=FNR )
    }
  }
  object
}

#' @title List pre-defined metrics for SummarizedBenchmark objects 
#' @aliases availableMetrics
#' @description
#' This function returns a data frame summarizing the default performance metrics provided in this package.
#' The data.frame contains three columns, `functions` is the name of the performance metric, `description`
#' is longer description of the performance metric and `requiredTruth` is logical depending on whether the
#' performance metrics require ground truths.
#' @examples
#' availableMetrics()
#'
#' @return A data.frame summarizing the default performance metrics provided in this package.
#' @md
#' @export
#' @author Alejandro Reyes
availableMetrics <- function(){
  data.frame(
    functions=c( "rejections", "TPR", "TNR", "FDR", "FNR",
                 "correlation", "sdad", "hamming", "LPnorm",
                 "adjustedRandIndex" ),
    description=c("Number of rejections", "True Positive Rate", "True Negative Rate",
                  "False Discovery Rate (estimated)", "False Negative Rate", "Pearson correlation",
                  "Standard Deviation of the Absolute Difference", "Hamming distance",
                  "L_{p} norm", "Adjusted Rand Index"),
    requiresTruth=rep( c(FALSE, TRUE), c( 1, 9 ) ) )
}

sb.TPR <- function( query, truth, alpha=0.1 ){
  sum( query <= alpha & truth == 1, na.rm = TRUE ) / sum( truth == 1, na.rm = TRUE )
}

sb.TNR <- function( query, truth, alpha=0.1 ){
  sum( ( !query <= alpha ) & truth == 0, na.rm = TRUE ) / sum( truth == 0, na.rm = TRUE )
}

sb.FDR <- function( query, truth, alpha=0.1 ){
  sum( query <= alpha & truth == 0, na.rm = TRUE ) / sum( query <= alpha, na.rm = TRUE )
}

sb.FNR <- function( query, truth, alpha=0.1 ){
  sum( !(query <= alpha) & truth == 1, na.rm = TRUE ) / sum( !( query <= alpha ), na.rm = TRUE )
}

sb.rejections <- function( query, truth, alpha=0.1 ){
  sum( query <= alpha, na.rm = TRUE )
}

sb.correlation <- function( query, truth, method="pearson" ){
  cor( query, truth, method=method )
}

sb.sdad <- function( query, truth ){
  sd( abs( query - truth ) )
}

sb.hamming <- function( query, truth ){
  sum( !query == truth, na.rm = TRUE )
}

sb.Lpnorm <- function( query, truth, p=2 ){
  sum( abs(query - truth) ^ p, na.rm = TRUE ) ^ (1/p)
}

#' @importFrom mclust adjustedRandIndex
sb.adjustedRandIndex <- function( query, truth ){
  adjustedRandIndex( query, truth )
}

assayHasTruths <- function( object, assay ){
  if( is.null( rowData(object)[[assay]] ) ){
    return(FALSE)
  }
  if( all( is.na( rowData( object )[[assay]] ) ) ){
    return(FALSE)
  }else{
    return(TRUE)
  }
}


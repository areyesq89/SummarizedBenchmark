#' @title availableMetrics
#' @aliases availableMetrics
#' @description
#' List default performance metrics available in this package.
#' @examples
#' availableMetrics()
#'
#' @return A data.frame with available performance metrics.
#' 
#' @export
availableMetrics <- function(){
  data.frame(
    functions=c( "rejections", "TPR", "TNR", "FPR", "FNR",
                 "correlation", "sdad", "hamming", "LPnorm",
                 "adjustedRandIndex" ),
    description=c("Number of rejections", "True Positive Rate", "True Negative Rate",
                  "False Positive Rate", "False Negative Rate", "Pearson correlation",
                  "Standard Deviation of the Absolute Difference", "Hamming distance",
                  "L_{p} norm", "Adjusted Rand Index"),
    requiresTruth=rep( c(FALSE, TRUE), c( 1, 9 ) ) )
}

sb.TPR <- function( query, truth, alpha=0.1 ){
  sum( query < alpha & truth == 1 ) / sum( truth == 1 )
}

sb.TNR <- function( query, truth, alpha=0.1 ){
  sum( ( !query < alpha ) & truth == 0 ) / sum( truth == 0 )
}

sb.FPR <- function( query, truth, alpha=0.1 ){
  sum( query < alpha & truth == 0 ) / sum( query < alpha )
}

sb.FNR <- function( query, truth, alpha=0.1 ){
  sum( !(query < alpha) & truth == 1 ) / sum( !( query < alpha ) )
}

sb.rejections <- function( query, truth, alpha=0.1 ){
  sum( query < alpha )
}

sb.correlation <- function( query, truth, method="pearson" ){
  cor( query, truth, method=method )
}

sb.sdad <- function( query, truth ){
  sd( abs( query - truth ) )
}

sb.hamming <- function( query, truth ){
  sum( !query == truth )
}

sb.Lpnorm <- function( query, truth, p=2 ){
  sum( abs(query - truth) ^ p ) ^ (1/p)
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

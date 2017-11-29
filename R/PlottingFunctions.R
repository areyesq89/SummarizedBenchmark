#' @title Calls UpSetR for qvalues of a \code{\link{SummarizedBenchmark}} object.
#' @aliases plotMethodsOverlap
#' @description
#' This function looks for an assay called 'qvalues' and given an alpha threshold,
#' it binarizes the qvalue matrix depending on whether the qvalues pass the alpha
#' threshold. Then it uses the function \code{\link{upset}} to plot the overlaps.
#'
#' @param object A \code{\link{SummarizedBenchmark}} object.
#' @param alpha An alpha value.
#' @param ... Further arguments passed to \code{\link{upset}}
#'
#' @examples
#' \dontrun{
#' data( sb )
#' plotMethodsOverlap(sb)
#' }
#'
#'
#' @author Alejandro Reyes
#'
#' @export
#' @importFrom UpSetR upset
#'
plotMethodsOverlap <- function( object, alpha=0.1, ... ){
  if( ! ( "qvalue" %in% assayNames(object) ) ){
    stop("The function 'plotOverlaps' requires an assay names 'qvalue'")
  }
  upset( as.data.frame( 1*( assays( object )[["qvalue"]] < alpha) ), ... )
}

#' @title ROC curve for 'qvalue' assays
#' @aliases plotROC
#' @description
#' This function inputs a \code{\link{SummarizedBenchmark}} object, looks
#' for an assay called 'qvalue' and plots receiver operating characteristic curves
#' for each of the methods to benchmark.
#'
#' @param object A \code{\link{SummarizedBenchmark}} object.
#'
#' @examples
#' \dontrun{
#' data( sb )
#' plotROC( sb )
#' }
#'
#' @return An \code{\link{SummarizedBenchmark}} object with performance functions.
#'
#' @author Alejandro Reyes
#' @export
#' @importFrom ggplot2 ggplot geom_line aes geom_abline xlim ylim
#'
plotROC <- function( object ){
  stopifnot( is( object, "SummarizedBenchmark" ))
  if( !any( assayNames( object ) %in% "qvalue" ) ){
    stop("Assay 'qvalue' not found.")
  }
  if( !assayHasTruths(object, "qvalue") ){
    stop("Ground truths not found for assay 'qvalue'")
  }
  metricsDf <- do.call( rbind, lapply( colnames(object), function(method){
    or <- order( assays(object)[["qvalue"]][,method] )
    TPR <- cumsum( groundTruths( object )[["qvalue"]][or] ) / sum( groundTruths(object)[["qvalue"]] )
    FPR <- cumsum( abs(groundTruths( object )[["qvalue"]][or] - 1) ) / seq_along(or)
    data.frame( method=method, TPR=TPR, FPR=FPR )
  } ) )
  pl <- ggplot( metricsDf, aes(FPR, TPR, col=method) ) +
    geom_line(size=1.2, alpha=.6) + xlim(0, 1) + ylim(0, 1) + geom_abline(linetype="dashed")
  pl
}

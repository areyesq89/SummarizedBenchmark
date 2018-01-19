#' @title Calls UpSetR for qvalues of a \code{\link{SummarizedBenchmark}} object.
#' @aliases plotMethodsOverlap
#' @description
#' This function looks for an assay, called by default 'qvalue', and given an alpha threshold,
#' it binarizes the assay matrix depending on whether its values are below the alpha
#' threshold. Then it uses the function \code{\link{upset}} to plot the overlaps.
#' The plot is only generated if at least 2 methods have observations that pass
#' the alpha threshold.
#'
#' @param object A \code{\link{SummarizedBenchmark}} object.
#' @param assay The name of an assay.
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
plotMethodsOverlap <- function( object, assay="qvalue", alpha=0.1, ... ){
  stopifnot( is( object, "SummarizedExperiment" ) )
  stopifnot( is( alpha, "numeric") )
  stopifnot( alpha >=0 & alpha <= 1 )
  if( !( assay %in% assayNames( object ) ) ){
    stop(sprintf("The function 'plotOverlaps' requires an assay names '%s'", assay) )
  }
  uobj <- as.data.frame( 1*( assays( object )[[assay]] < alpha) )
  if ( sum(colSums(uobj) > 0) < 2 ){
    stop("To plot overlaps, at least 2 methods must have observations that pass the alpha threshold.")
  }
  upset(uobj , ... )
}

#' @title Plotting ROC curves
#' @aliases plotROC
#' @description
#' This function inputs a \code{\link{SummarizedBenchmark}} object, looks
#' for an assay called 'qvalue' and plots receiver operating characteristic curves
#' for each of the methods to benchmark.
#'
#' @param object A \code{\link{SummarizedBenchmark}} object.
#' @param assay An assay name.
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
plotROC <- function( object, assay="qvalue" ){
  stopifnot( is( object, "SummarizedBenchmark" ))
  if( !any( assayNames( object ) %in% assay ) ){
    stop(sprintf("Assay '%s' not found.\n", assay) )
  }
  if( !assayHasTruths( object, assay ) ){
    stop(sprintf("Ground truths not found for assay '%s'", assay))
  }
  metricsDf <- do.call( rbind, lapply( colnames(object), function(method){
    or <- order( assays(object)[[assay]][,method] )
    TPR <- cumsum( groundTruths( object )[[assay]][or] ) / sum( groundTruths(object)[[assay]] )
    FPR <- cumsum( abs(groundTruths( object )[[assay]][or] - 1) ) / seq_along(or)
    data.frame( method=method, TPR=TPR, FPR=FPR )
  } ) )
  pl <- ggplot( metricsDf, aes(FPR, TPR, col=method) ) +
    geom_line(size=1.2, alpha=.6) + xlim(0, 1) + ylim(0, 1) + geom_abline(linetype="dashed")
  pl
}

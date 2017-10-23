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
#' @return An \code{\link{SummarizedBenchmark}} object with performance functions.
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

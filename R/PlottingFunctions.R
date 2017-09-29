#' @export
#' @author Alejandro Reyes
#' @importFrom UpSetR upset
#'
plotOverlaps <- function( object, alpha=0.1, ... ){
  if( ! ( "qvalue" %in% assayNames(object) ) ){
    stop("The function 'plotOverlaps' requires an assay names 'qvalue'")
  }
  upset( as.data.frame( 1*(assays( object )[["qvalue"]] < alpha) ), ... )
}

plotOverlaps( object, alpha=0.5, order.by="freq" )

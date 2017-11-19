#' @rdname addDefaultMetrics
#' @aliases availableMetrics
#' @description
#' List default performance metrics available in this package.
#'
#' @export
availableMetrics <- function(){
  data.frame(
    functions=c( "rejections", "TPR", "TNR", "FPR", "FNR" ),
    assays=rep( "qvalue", 5 ),
    requiresTruth=rep( c(FALSE, TRUE), c(1, 4) )
  )
}

TPR <- function( query, truth, alpha=0.1 ){
  sum( query < alpha & truth == 1 ) / sum( truth == 1 )
}

TNR <- function( query, truth, alpha=0.1 ){
  sum( ( !query < alpha ) & truth == 0 ) / sum( truth == 0 )
}

FPR <- function( query, truth, alpha=0.1 ){
  sum( query < alpha & truth == 0 ) / sum( query < alpha )
}

FNR <- function( query, truth, alpha=0.1 ){
  sum( !(query < alpha) & truth == 1 ) / sum( !( query < alpha ) )
}

rejections <- function( query, truth, alpha=0.1 ){
  sum( query < alpha )
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

#' @title Add default performance metrics to a \code{\link{SummarizedBenchmark}} object.
#' @aliases addDefaultMetrics
#' @description
#' This function adds predefined performance metrics to a \code{\link{SummarizedBenchmark}}
#' object, according to the assay name. For example, if an assay is named "qvalue",
#' it will add a the performance metric "rejections".
#'
#' @param object A \code{\link{SummarizedBenchmark}} object.
#' @param metrics A character vector with performance metric names to be added.
#' See function \code{availableMetrics}.
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
#' "FNR" (false negative rate) will be added. See function \code{availableMetrics()}
#' to retrieve a complete list of metrics defined in this package.
#'
#' @author Alejandro Reyes
#'
#' @export
addDefaultMetrics <- function( object, metrics=NULL ){
  defaultMetrics <- availableMetrics()
  if( !is.null( metrics ) ){
    stopifnot( metrics %in% defaultMetrics$functions )
    defaultMetrics <- defaultMetrics[defaultMetrics$functions %in% metrics,]
  }
  for( j in assayNames(object) ){
    defaultMetricsForAssay <- defaultMetrics[defaultMetrics$assays %in% j,]
    if( nrow(defaultMetricsForAssay) == 0 ){ next }
    if( !assayHasTruths( object, j ) ){
      defaultMetricsForAssay <- defaultMetricsForAssay[!defaultMetrics$requiresTruth,]
    }
    if( nrow(defaultMetricsForAssay) == 0 ){ next }
    for( i in as.character(defaultMetricsForAssay$functions) ){
      object <- addPerformanceMetric(
        object=object,
        assay=j,
        evalMetric=i,
        evalFunction = get(i) )
    }
  }
  object
}

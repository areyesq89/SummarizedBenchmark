#' @title Add a performance metric definition to a \code{\link{SummarizedBenchmark}} object.
#' @aliases addPerformanceMetric
#' @description
#' This is a function to define performance metrics for benchmarking methods.
#' The function is saved into the \code{performanceMetrics} slot.
#'
#' @param object A \code{\link{SummarizedBenchmark}} object.
#' @param evalMetric A string with the name of the evaluation metric.
#' @param assay A string with an assay name. Indicates the assay that should be
#' given as input to this performance metric.
#' @param evalFunction A function that calculates a performance metric. It should
#' contain at least two arguments, query and truth, where query is the output vector
#' of a method and truth is the vector of true values. If additional parameters
#' are specified, they must contain default values.
#' 
#' @author Alejandro Reyes
#'
#' @examples
#'
#' data( sb )
#' sb <- addPerformanceMetric(
#'    object=sb,
#'    assay="qvalue",
#'    evalMetric="TPR",
#'    evalFunction = function( query, truth, alpha=0.1 ){
#'        goodHits <- sum( (query < alpha) & truth == 1 )
#'        goodHits / sum(truth == 1)
#'    }
#' )
#'
#' @return A \code{\link{SummarizedBenchmark}} object.
#' @importFrom tidyr gather
#' @export
#'
addPerformanceMetric <- function( object, evalMetric, assay, evalFunction ){
  validObject( object )
  if( is.null( evalMetric ) ){
    stop("Please specify a name for the new evaluation metric (parameter evalMetric).")
  }
  if( is.null( assay ) ){
    stop("Please specify an assay for the new evaluation metric (parameter assay).")
  }
  if( !assay %in% names(object@performanceMetrics) ){
    stop(sprintf( "Assay '%s' not found in this object", assay) )
  }
  if( !length( assay ) == 1 ){
    stop("The 'assay' parameter must be of length 1")
  }
  if( !length( evalMetric ) == 1 ){
    stop("The 'evalMetric' parameter must be of length 1")
  }
  object@performanceMetrics[[assay]][[evalMetric]] <- evalFunction
  validObject( object )
  object
}


#' @describeIn estimateMetrics Estimate performance metrics for a given assay
#' @title Estimate performance metrics.
#' @aliases estimateMetricsForAssay
#' @description
#' These functions estimate the performance metrics, either passed as arguments or
#' added previously with the \code{\link{addPerformanceMetric}} function. The function
#' will estimate the performance metric for each method.
#'
#' @param object A \code{\link{SummarizedBenchmark}} object.
#' @param assay A string with an assay name. Indicates the assay that should be
#' given as input to this performance metric.
#' @param evalMetric A string with the name of the evaluation metric.
#' @param evalFunction A function that calculates a performance metric. It should
#' contain at least two arguments, query and truth, where query is the output vector
#' of a method and truth is the vector of ground true values. If additional parameters
#' are specified, they must contain default values. If this parameter is passed,
#' the metrics in the object are ignored and only this evaluation metric
#' is estimated.
#' @param addColData Logical (default: FALSE). If TRUE, the results are added to the
#' \code{\link{colData}} slot of the \code{\link{SummarizedExperiment}} object and
#' the object is returned. If FALSE, only a \code{\link{DataFrame}} with the results
#' is returned.
#' @param tidy Logical (default: FALSE). If TRUE, a long formated \code{\link{data.frame}}
#' is returned.
#' @param ... Additional parameters passed to the performance functions.
#' 
#' @author Alejandro Reyes
#'
#' @examples
#'
#' data( sb )
#' sb <- addPerformanceMetric(
#'    object=sb,
#'    assay="qvalue",
#'    evalMetric="TPR",
#'    evalFunction = function( query, truth, alpha=0.1 ){
#'        goodHits <- sum( (query < alpha) & truth == 1 )
#'        goodHits / sum(truth == 1)
#'    }
#' )
#'
#' qvalueMetrics <- estimateMetricsForAssay( sb, assay="qvalue" )
#' allMetrics <- estimatePerformanceMetrics( sb )
#' allMetricsTidy <- estimatePerformanceMetrics( sb, tidy=TRUE )
#'
#' @return Either a \code{\link{SummarizedBenchmark}} object, a \code{\link{DataFrame}} or
#' a \code{\link{data.frame}}.
#' @export
#'
estimateMetricsForAssay <- function( object, assay, evalMetric=NULL, addColData=FALSE,
                                     evalFunction=NULL, tidy=FALSE, ...){
  stopifnot( is( object, "SummarizedBenchmark" ) )
  validObject( object )
  if( !all( assay %in% names( assays( object ) ) ) ){
    stop(sprintf("Assay '%s' not found in SummarizedBenchmark", assay))
  }
  if( !is.null(evalFunction) ){
    object <- addPerformanceMetric( object, evalMetric, assay, evalFunction )

  }
  allFunctions <- object@performanceMetrics[[assay]]
  if( length(allFunctions) == 0 ){
    stop(sprintf( "Metric functions not specified for assay(s): %s. Check `?addPerformanceMetric`.", assay ) )
  }
  if( !is.null( evalMetric ) ){
    if( !all(evalMetric %in% names(allFunctions)) ){
      notDefined <- evalMetric[!evalMetric %in% names(allFunctions)]
      stop(sprintf("Function for metric(s) %s is/are not defined", paste(notDefined, collapse=",")))
    }
    allFunctions <- allFunctions[names( allFunctions ) %in% evalMetric]
  }
  allDotArgs <- as.list( match.call( expand.dots=FALSE ) )[["..."]]
  assayData <- assays(object)[[assay]]
  assayTruth <- rowData(object)[,assay]
  res <- lapply( names( allFunctions ), function( nf ){
    f <- allFunctions[[nf]]
    vecArgs <- formalArgs( f )[ !formalArgs( f ) %in% c("query", "truth") ]
    passArgs <- list(truth=assayTruth)
    eleMD <- DataFrame( colType="performanceMetric", assay=assay, performanceMetric=nf )
    if( length( vecArgs ) > 0 ){
      vf <- Vectorize( f, vectorize.args=vecArgs )
      defaultArgs <- formals(f)[vecArgs[!vecArgs %in% names( passArgs )]]
      extraDotArgs <- allDotArgs[names(allDotArgs) %in% formalArgs(f)]
      if( length( extraDotArgs ) > 0 ){
        for( i in names( extraDotArgs ) ){
          defaultArgs[[i]] <- eval( extraDotArgs[[i]] )
        }
      }
      passArgs <- c( passArgs, defaultArgs )
      resNRow <- max( sapply( passArgs[vecArgs], length ) )
      eleMD <- cbind( eleMD, DataFrame( as.data.frame( passArgs[vecArgs] ) ) )
    }else{
      vf <- f
      resNRow <- 1
    }
    indRes <- sapply( seq_len( ncol( assayData ) ), function( i ){
      assayRes <- do.call( vf, c( list(query=assayData[,i]), passArgs ) )
      assayRes
    } )
    resDF <- DataFrame( t( matrix( indRes, nrow=resNRow ) ) )
    elementMetadata( resDF ) <- eleMD
    if( resNRow > 1 ){
      colnames( resDF ) <- paste( nf, seq_len( ncol( resDF ) ), sep=".")
    }else{
      colnames( resDF ) <- nf
    }
    resDF
  } )
  names( res ) <- names( allFunctions )
  res <- Reduce( cbind, res )
  res <- cbind( colData( object ), res )
  if( addColData | tidy ){
    colData( object ) <- res
    if( tidy ){
      return( tidyUpMetrics( object ) )
    }else{
      return( object )
    }
  }else{
    return( res )
  }
}

#' @describeIn estimateMetrics Estimate performance metrics for all assays
#' @aliases estimatePerformanceMetrics
#' @export
estimatePerformanceMetrics <- function( object, addColData=FALSE, tidy=FALSE, ... ){
  stopifnot( is( object, "SummarizedBenchmark" ) )
  validObject( object )
  assayNames <- assayNames( object )
  allRes <- lapply( assayNames, function(x){
    if( length( object@performanceMetrics[[x]] ) > 0 ){
      estimateMetricsForAssay( object, assay=x, ... )
    }else{
      NULL
    }
  } )
  allRes <- allRes[!sapply(allRes, is.null)]
  if( length( allRes ) > 0 ){
    allRes <-
      lapply( allRes,
              function(x){
                x[,elementMetadata( x )$colType == "performanceMetric",drop=FALSE]
              } )
    allRes <- Reduce( cbind, allRes )
  }else{
    stop("Metric functions not found for any assay. Check `?addPerformanceMetric`. to include these." )
  }
  allRes <- cbind( colData(object), allRes )
  if( addColData | tidy ){
    colData(object) <- allRes
    if( tidy ){
      return( tidyUpMetrics( object ) )
    }else{
      return( object )
    }
  }else{
    return( allRes )
  }
}

#' @title Reformat performance metrics to a long format.
#' @aliases tidyUpMetrics
#' @description
#' This function takes as input a \code{SummarizedBenchmark} object, extracts the
#' estimated performance metrics and reformats them into a long-formated data frame.
#'
#' @param object A \code{\link{SummarizedBenchmark}} object.
#' @examples
#' data( "sb", package="SummarizedBenchmark" )
#' sb <- estimateMetricsForAssay( sb, assay="qvalue", evalMetric="rejections",
#'     evalFunction=function( query, truth, alpha=0.1 ){
#'         sum( query < alpha )
#'     },
#'     addColData=TRUE )
#' tidyUpMetrics( sb )
#'
#' @author Alejandro Reyes
#'
#' @return A tidy \code{data.frame}
#' @importFrom tidyr gather
#' @export
#'
tidyUpMetrics <- function( object ){
  stopifnot(is( object, "SummarizedBenchmark" ) )
  validObject( object )
  res <- colData( object )
  isPerformanceMetric <- elementMetadata( res )$colType == "performanceMetric"
  if( !sum(isPerformanceMetric) > 0 ){
    stop("No performance metrics were found. Check ?estimatePerformanceMetrics for further information")
  }
  valueCols <- colnames(res)[isPerformanceMetric]
  tidyRes <- gather( as.data.frame(res), keys=valueCols )
  mData <- as.data.frame( elementMetadata(res)[isPerformanceMetric,] )
  rownames(mData) <- valueCols
  mData[["colType"]] <- NULL
  for( i in colnames(mData) ){
    varAdd <- mData[[i]]
    names(varAdd) <- rownames(mData)
    tidyRes[[i]] <- varAdd[tidyRes$key]
  }
  tidyRes
}

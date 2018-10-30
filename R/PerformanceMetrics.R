#' @title Add performance metric to SummarizedBenchmark object
#' @aliases addPerformanceMetric
#' @description
#' This is a function to define performance metrics for benchmarking methods.
#' The function is saved into the \code{performanceMetrics} slot.
#'
#' @param object A \code{\link[=SummarizedBenchmark-class]{SummarizedBenchmark}} object.
#' @param evalMetric A string with the name of the evaluation metric.
#' @param assay A string with an assay name. Indicates the assay that should be
#' given as input to this performance metric.
#' @param evalFunction A function that calculates a performance metric. It should
#' contain at least two arguments, query and truth, where query is the output vector
#' of a method and truth is the vector of true values. If additional parameters
#' are specified, they must contain default values. If NULL, the 'evalMetric' string must
#' be the name of a predefined metric available through 'availableMetrics()$function'.
#'
#' @author Alejandro Reyes
#'
#' @examples
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
#' @seealso \code{\link{availableMetrics}}, \code{\link{performanceMetrics}}
#' @return A \code{\link[=SummarizedBenchmark-class]{SummarizedBenchmark}} object.
#' @importFrom tidyr gather
#' @export
#'
addPerformanceMetric <- function( object, evalMetric, assay, evalFunction=NULL ){
  stopifnot( is(object, "SummarizedBenchmark") )
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
  if( is( evalFunction, "function" ) ){
    if( !length( evalMetric ) == 1 ){
      stop("The 'evalMetric' parameter must be of length 1")
    }
    object@performanceMetrics[[assay]][[evalMetric]] <- evalFunction
  }else if( is.null( evalFunction ) ){
    stopifnot( all( evalMetric %in% availableMetrics()$functions ) )
    for( i in evalMetric ){
      object@performanceMetrics[[assay]][[i]] <- get( paste0( "sb.", i ) )
    }
  }
  validObject( object )
  object
}

is.scalar <- function(x){
  is.atomic(x) && length(x) == 1L
}

#' @describeIn estimateMetrics Estimate performance metrics for a given assay
#' @title Estimate performance metrics in SummarizedBenchmark object
#' @aliases estimateMetricsForAssay
#' @description
#' These functions estimate the performance metrics, either passed as arguments or
#' added previously with the \code{\link{addPerformanceMetric}} function. The function
#' will estimate the performance metric for each method.
#'
#' @param object A \code{\link[=SummarizedBenchmark-class]{SummarizedBenchmark}} object.
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
#' @param rerun Logical (default: TRUE). By default, all performance metrics are recalculated
#' everytime that \code{\link{estimatePerformanceMetrics}} is called. If FALSE, performance metrics
#' will only be calculated for newly added methods or modified methods.
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
#' @seealso \code{\link{availableMetrics}}, \code{\link{performanceMetrics}}
#' @return Either a \code{\link{SummarizedBenchmark}} object, a \code{\link{DataFrame}} or
#' a \code{\link{data.frame}}.
#' @importFrom S4Vectors elementMetadata
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
#    vf <- Vectorize( f, vectorize.args=vecArgs, SIMPLIFY=FALSE)
    if( length( vecArgs ) > 0 ){
      vf <- Vectorize( f, vectorize.args=vecArgs, SIMPLIFY=FALSE)
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
    indRes <- lapply( seq_len( ncol( assayData ) ), function( i ){
      assayRes <- do.call( vf, c( list(query=assayData[,i]), passArgs ) )
      if( all( is.na( assayData[,i] ) ) ){
        rep( NA, length( assayRes ) )
      }else{
        assayRes
      }
    } )
    resDF <- DataFrame( row.names=colnames(assayData) )
    if( length(vecArgs) > 0 ){
      if( resNRow > 1 ){
        resColNames <- paste( nf, seq_len( resNRow ), sep="." )
      }else{
        resColNames <- nf
      }
      for( i in seq_len( length(resColNames) ) ){
        resInd <- lapply( indRes, "[[", i )
        if( all( sapply( resInd, is.scalar ) ) ){
          resInd <- unlist(resInd, recursive=FALSE)
        }
        resDF[[resColNames[i]]] <- resInd
      }
    }else{
      resColNames <- nf
      resDF[[resColNames]] <- indRes
    }
    elementMetadata( resDF ) <- eleMD
    resDF
  } )
  names( res ) <- names( allFunctions )
  res <- Reduce( cbind, res )
  object <- cleanPerformanceMetrics( object )
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
estimatePerformanceMetrics <- function( object, addColData=FALSE, tidy=FALSE, rerun=TRUE, ... ){
  stopifnot( is( object, "SummarizedBenchmark" ) )
  validObject( object )
  assayNames <- assayNames( object )
  if( addColData & !rerun & !is.null( colData(object)$pm.session ) ){
    rerunMeth <- colData( object )$session.idx != colData( object )$pm.session |
      is.na( colData( object )$pm.session )
    if( sum( rerunMeth ) < 1L ){
      stop("All performance metrics appear to be up-to-date.")
    }else{
      cat( sprintf("\nOption rerun is set to `FALSE`:\nRerunning performance metrics only for the following methods: %s\n\n",
                   paste( colnames(object)[rerunMeth], collapse=", " ) ) )
    }
    objectPrev <- object
    object <- object[,rerunMeth]
  }
  object <- cleanPerformanceMetrics( object )
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
                x[,elementMetadata( x )$colType == "performanceMetric" &
                    !is.na( elementMetadata( x )$colType ),drop=FALSE]
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
      if( !rerun ){
        newEvals <- colData(object)
        colData(objectPrev)[rownames(newEvals),colnames(newEvals)] <- newEvals
        object <- objectPrev
      }
      colData( object )$pm.session <- colData( object )$session.idx
      idx <- colnames( colData(object) ) %in% "pm.session"
      elementMetadata( colData(object) )[idx,"colType"] <- "performanceMetricSession"
      return( object )
    }
  }else{
    return( allRes )
  }
}

cleanPerformanceMetrics <- function( object ){
  prevMetrics <- elementMetadata( colData( object ) )$colType == "performanceMetric" &
    !is.na( elementMetadata( colData( object ) )$colType )
  if( any( prevMetrics ) ){
    message("Found already estimated performance metrics, replacing these")
    colData( object ) <- colData( object )[,!prevMetrics,drop=FALSE]
  }
  object
}

#' @title Tidy up performance metrics in SummarizedBenchmark object
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
#' @seealso \code{\link{estimatePerformanceMetrics}}
#' @return A tidy \code{data.frame}
#' @importFrom tidyr gather
#' @export
#'
tidyUpMetrics <- function( object ){
  stopifnot(is( object, "SummarizedBenchmark" ) )
  validObject( object )
  res <- colData( object )
  isPerformanceMetric <- elementMetadata( res )$colType == "performanceMetric" &
    !is.na( elementMetadata( colData( object ) )$colType )
  if( !sum(isPerformanceMetric) > 0 ){
    stop("No performance metrics were found. Check ?estimatePerformanceMetrics for further information")
  }
  valueCols <- colnames(res)[isPerformanceMetric]
  tidyRes <- data.frame(res, label = rownames(res), check.names = TRUE)
  tidyRes <- gather( tidyRes, keys=valueCols )
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

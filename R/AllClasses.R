setOldClass(c("quosure", "quosures"))
setClassUnion("listOrNULL", c("list", "NULL"))
setClassUnion("listOrCharacter", c("list", "character"))

#' BDMethod class
#'
#' Container for individual methods to be compared as part of
#' a BenchDesign object.
#'
#' @slot data a list or MD5 hash of the data.
#' @slot type a character string indicating whether the data slot
#'       contains the 'data' or a 'hash' of the data.
#'
#' @exportClass BDData
#' @name BDData-class
#' @author Patrick Kimes
setClass("BDData", representation(data = "listOrCharacter", type = "character"))

setValidity("BDData",
            function(object) {
                if (! object@type %in% c("data", "hash"))
                    stop("The type must be one of 'data' or 'hash'.")
                if (object@type == "data" & !is.list(object@data))
                    stop("The data must be a list if type = 'data'.")
                if (object@type == "hash" & !is(object@data, "character"))
                    stop("The data must be a character string if type = 'hash'.")
                TRUE
            })

setClassUnion("BDDataOrNULL", c("BDData", "NULL"))

#' BDMethod class
#'
#' Container for individual methods to be compared as part of
#' a BenchDesign object.
#'
#' @slot f a function to be benchmarked
#' @slot fc a captured expression of the function \code{f}
#' @slot params a list of quosures specifying function parameters
#' @slot post a list of functions to be applied to the output of \code{f}
#' @slot meta a list of meta data
#'
#' @exportClass BDMethod
#' @name BDMethod-class
#' @author Patrick Kimes
setClass("BDMethod", representation(f = "function", fc = "language", params = "quosures",
                                    post = "listOrNULL", meta = "listOrNULL"))

setValidity("BDMethod",
            function(object) {
                post <- object@post
                meta <- object@meta
                if (!is.null(post) && is.null(names(post)))
                    stop("The slot 'post' must be a named list, else should be NULL.")
                if (!is.null(post) && any(names(post) == ""))
                    stop("The slot 'post' must be a named list, else should be NULL.")
                if (!is.null(post) && !all(sapply(post, is, "function")))
                    stop("The slot 'post' must be a list of functions, else should be NULL.")
                if (!is.null(meta) && is.null(names(meta)))
                    stop("The slot 'meta' must be a named list, else should be NULL.")
                if (!is.null(meta) && any(names(meta) == ""))
                    stop("The slot 'meta' must be a named list, else should be NULL.")
                TRUE
            })


#' BenchDesign class
#'
#' Container for benchmark methods and data.
#'
#' @slot data a list containing the data to be used in the benchmark.
#' @slot methods a list of \code{BDMethods} to be compared in the benchmark.
#'
#' @exportClass BenchDesign
#' @name BenchDesign-class
#' @author Patrick Kimes
setClass("BenchDesign", representation(data = "BDDataOrNULL", methods = "list"))

setValidity("BenchDesign",
            function(object) {
                if (length(object@methods) > 0 && is.null(names(object@methods)))
                    stop("The slot 'methods' must be a named list of BDMethods, else should be length zero.")
                if (length(object@methods) > 0 && !all(sapply(object@methods, is, "BDMethod")))
                    stop("The slot 'methods' must be a named list of BDMethods, else should be length zero.")
                TRUE
            })


setClassUnion("BenchDesignOrNULL", c("BenchDesign", "NULL"))

#' @name SummarizedBenchmark-class
#' @title SummarizedBenchmark class documentation
#' @description
#' Extension of the \code{\link{RangedSummarizedExperiment}} to
#' store the output of different methods intended for the same purpose
#' in a given dataset. For example, a differential expression analysis could be
#' done using \pkg{limma}-voom, \pkg{edgeR} and \pkg{DESeq2}. The
#' SummarizedBenchmark class provides a framework that is useful to store, benckmark and
#' compare results.
#' @slot performanceMetrics A \code{\link{SimpleList}} of the same length
#' as the number of \code{\link{assays}} containing performance
#' functions to be compared with the ground truths.
#'
#' @author Alejandro Reyes
#'
#' @aliases SummarizedBenchmark-class
#' @importClassesFrom SummarizedExperiment RangedSummarizedExperiment
#' @importFrom methods as formalArgs is new validObject
#' @import S4Vectors SummarizedExperiment
#' @export
#' 
#' @examples
#'
#' ## loading the example data from iCOBRA
#' library(iCOBRA)
#' data(cobradata_example)
#'
#' ## a bit of data wrangling and reformatting
#' assays <- list(
#'     qvalue=cobradata_example@padj,
#'     logFC=cobradata_example@score )
#' assays[["qvalue"]]$DESeq2 <- p.adjust(cobradata_example@pval$DESeq2, method="BH")
#' groundTruth <- DataFrame( cobradata_example@truth[,c("status", "logFC")] )
#' colnames(groundTruth) <- names( assays )
#' colData <- DataFrame( method=colnames(assays[[1]]) )
#' groundTruth <- groundTruth[rownames(assays[[1]]),]
#'
#' ## constructing a SummarizedBenchmark object
#' sb <- SummarizedBenchmark(
#'     assays=assays, colData=colData,
#'     groundTruth=groundTruth )
#'
#' @exportClass SummarizedBenchmark
setClass( "SummarizedBenchmark",
         contains = "RangedSummarizedExperiment",
         representation = representation(performanceMetrics = "SimpleList",
                                         BenchDesign = "BenchDesignOrNULL"))

setValidity( "SummarizedBenchmark", function( object ) {
  if( ! length( object@performanceMetrics ) == length( assays( object ) ) ){
    stop("The number of elements of the slot 'performanceFunction' has
         be of the same length as the number of assays.")
  }
  if( !all( names( object@performanceMetrics ) %in% assayNames( object  ) ) ){
    stop("The names of the performanceMetrics list must match the names of the assays")
  }
  if( !all( sapply( object@performanceMetrics, "class" ) == "list" ) ){
    stop("In the slot 'performanceMetrics', each element of the list must contain a list of functions")
  }
  permFunc <- object@performanceMetrics
  for( i in names( permFunc ) ){
    funcAssay <- permFunc[[i]]
    for(j in names(funcAssay) ){
      y <- funcAssay[[j]]
      f1 <- all( c("query", "truth") %in% formalArgs( y ) )
      if( !f1 ){
        stop(sprintf( "The performance function '%s' for assay '%s' does not contain a 'query' and/or a 'truth' argument", j, i) )
      }
      otherArgs <- formals(y)[!names(formals(y)) %in% c("query", "truth")]
      f2 <- any( sapply(otherArgs, class) == "name" )
      if( f2 ){
        missingDefaults <- names(otherArgs)[which(f2)]
        stop(sprintf("The parameter(s) '%s' of the performance function '%s' for assay '%s' has/have no default values. Please specify defaults values.", paste(missingDefaults, collapse=","), j, i ) )
      }
    }
  }
  TRUE
} )

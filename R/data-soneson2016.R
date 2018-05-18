#' SummarizedBenchmark example
#'
#' @name sb
#' @description This object contains the example data from the \code{iCOBRA} package reformatted as a
#' \code{SummarizedBenchmark} object. It consists of differential expression results from \code{DESeq2}
#' \code{edgeR} and \code{limma}-voom.
#' @source Example data from the \code{iCOBRA} package.
#' @aliases truthdat txi
#' @docType data
#' @keywords data SummarizedBenchmark
#' @examples
#' data( sb )
NULL

#' SummarizedBenchmark object of isoform quantification results
#'
#' @name allSB
#' @aliases allSB,quantSB
#' @description This object is a \code{SummarizedBenchmark} object containing isoform quantifications from salmon, sailfish and kallisto
#' from 4 mouse samples (2 hearts and 2 brains) part of the Mouse BodyMap. Its generation is described in one of the vignettes of this package.
#' @source Mouse BodyMap (Li et al, 2014). SRA accession numbers SRR5273705, SRR5273689, SRR5273699 and SRR5273683.
#' @docType data
#' @keywords data
#' @examples
#' data( quantSB )
NULL

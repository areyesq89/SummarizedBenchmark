#' Create a new BDData object
#'
#' @description
#' Initializes a new BDData object of benchmarking data.
#'
#' Data sets are stored as BDData objects within BenchDesign
#' objects as well as SummarizedBenchmark objects. However, because
#' data is directly specified to the BenchDesign constructor, there
#' is usually no need to call the BDData constructor to create completely
#' new data objects.
#'
#' The BDData constructor is most useful for extracting the data sets
#' contained in BenchDesign objects as well as SummarizedBenchmark objects.
#' By default, the BDData object stored in SummarizedBenchmark objects will
#' be MD5 hashes rather than the complete original data set. 
#' \code{\link{compareBDData}} can be used to compare various forms of
#' BDData, as shown in the examples below.
#' 
#' @param data a list object of data or MD5 hash string
#'
#' @return
#' BDData object
#'
#' @examples
#' ## construct from data.frame
#' datadf <- data.frame(x = 1:5, y = runif(5))
#' bdd_df <- BDData(datadf)
#' bdd_df
#'
#' ## construct from MD5 hash of data.frame
#' bdd_md5 <- BDData(digest::digest(datadf))
#' bdd_md5
#'
#' ## compare two BDData objects 
#' compareBDData(bdd_df, bdd_md5)
#'
#' ## note that the data is the same, i.e. the MD5 hashes match, but the
#' ## data types ("data" vs. "md5has") are different
#' 
#' @seealso \code{\link{BDData-class}}, \code{\link{BenchDesign}}
#' @name BDData
#' @export
#' @author Patrick Kimes
NULL

.BDData.default <- function(data) {
    if (is(data, "character")) {
        new("BDData", data = data, type = "md5hash")
    } else {
        new("BDData", data = data, type = "data")
    }
}

.BDData.bd <- function(data) {
    data@data
}

.BDData.sb <- function(data) {
    BDData(BenchDesign(data))
}

#' @rdname BDData
setMethod("BDData", signature(data = "ANY"), .BDData.default)

#' @rdname BDData
setMethod("BDData", signature(data = "BenchDesign"), .BDData.bd)

#' @rdname BDData
setMethod("BDData", signature(data = "SummarizedBenchmark"), .BDData.sb)

#' @rdname BDData
setMethod("BDData", signature(data = "BDData"), function(data) { data })


#' Hash data in BDData object 
#'
#' @description
#' Repliaces data stored in a \code{\link[=BDData-class]{BDData}} object
#' with the MD5 hash of the data. If the data was already a MD5 hash, the
#' original object is returned unchanged. The method can be called directly
#' on \code{\link[=BenchDesign-class]{BenchDesign}} objects to hash the
#' underlying data as well.
#'
#' @param object a \code{BDData} or \code{BenchDesign} object
#'
#' @return
#' an object of the same class as \code{object} with data
#' converted to a MD5 hash.
#'
#' @name hashBDData
#' @importFrom digest digest
#' @export
#' @author Patrick Kimes
NULL

.hashBDData <- function(object) {
    if (object@type == "md5hash")
        return(object)
    BDData(digest::digest(object@data, algo = "md5"))
}

.hashBDData.bd <- function(object) {
    BDData(object) <- hashBDData(BDData(object))
    object
}

#' @rdname hashBDData
setMethod("hashBDData", signature(object = "BDData"), .hashBDData)

#' @rdname hashBDData
setMethod("hashBDData", signature(object = "BenchDesign"), .hashBDData.bd)



#' Create a new BDData
#'
#' Initialized a new BDData object for benchmarking methods.
#'
#' @param data a list object of data or MD5 hash string
#'
#' @return
#' BDData object
#'
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


#' Convert BDData to BDData with MD5 Hash 
#'
#' Simple converting function to replace data object in
#' BDData object with MD5 hash value.
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



## Compare BDMethod objects
#'
#' Simple comparison of two BDMethod objects based on
#' meta data.
#' 
#' @param bdm1 a \code{BDMethod} object
#' @param bdm2 a \code{BDMethod} object
#'
#' @return
#' logical value indicating whether the two objects produced the same
#' meta data.
#'
#' @export
#' @importFrom dplyr all_equal
#' @author Patrick Kimes
compareBDMethods <- function(bdm1, bdm2) {
    if(!is(bdm1, "BDMethod") || !is(bdm2, "BDMethod"))
        stop("Must specify two BDMethod objects to compare.")
    dplyr::all_equal(tidyBDMethod(bdm1), tidyBDMethod(bdm2))
}

#' Compare BenchDesign objects
#'
#' Simple comparison of two BenchDesign objects based on
#' methods meta data and data MD5 hashs.
#'
#' @param bd1 a \code{BenchDesign} object
#' @param bd2 a \code{BenchDesign} object
#'
#' @return
#' logical value indicating whether the two objects have
#' methods producing the same meta data and data with the
#' same MD5 hashes.
#' 
#' @export
#' @author Patrick Kimes
compareBenchDesigns <- function(bd1, bd2) {
    if (!is(bd1, "BenchDesign") || !is(bd2, "BenchDesign"))
        stop("Must specify two BenchDesign objects to compare.")

    ## fill out
    
    return(TRUE)
}

#' Convert BDData to BDData with MD5 Hash 
#'
#' Simple converting function to replace data object in
#' BDData object with MD5 hash value.
#'
#' @param bdd a \code{BDData} object
#'
#' @return
#' \code{BDData} object with type = "md5hash".
#' 
#' @importFrom digest digest
#' @export
#' @author Patrick Kimes
BDDataHash <- function(bdd) {
    stopifnot(is(bdd, "BDData"))
    if (bdd@type == "md5hash")
        return(bdd)
    new("BDData", data = digest::digest(bdd@data, algo = "md5"),
        type = "md5hash")
}


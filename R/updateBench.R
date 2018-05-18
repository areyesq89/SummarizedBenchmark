#' Update SummarizedBenchmark object
#'
#' Update SummarizedBenchmark results with new methods.
#' 
#' @param sb a \code{SummarizedBenchmark} object
#' @param bd a \code{BenchDesign} object
#' @param dryrun logical whether to just print description of what would
#'        be updated rather than actually running any methods. (default = TRUE)
#' @param version logical whether to re-run methods with only package
#'        version differences. (default = FALSE)
#' @param ... optional parameters to pass to \code{\link{buildBench}}.
#' 
#' @return
#' SumamrizedBenchmark object.
#'
#' @export
#' @author Patrick Kimes
updateBench <- function(sb, bd = NULL, dryrun = TRUE, version = FALSE, ...) {
    ## capture buildBench parameters
    bbp <- list(...)
    if (data %in% names(bbp))
        bd@data <- BDData(bbp$data)

    ## .... TBD

    return(sb)
}

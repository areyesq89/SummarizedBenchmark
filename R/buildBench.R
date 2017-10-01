#' Make SummarizedBenchmark from BenchDesign
#'
#' Function to evaluate BenchDesign methods on supplied
#' data set to generate a SummarizedBenchmark.
#' 
#' @param b BenchDesign object
#' 
#' @return
#' SummarizedBenchmark with one assay
#'
#' @export
#' @author Patrick Kimes
buildBench <- function(b) {
    
    ## assay: evaluate all functions
    a <- eval_defaults(b)
    a <- do.call(cbind, a)
    a <- list("bench" = a)
    
    ## colData: method information
    df <- clean_methods(b)
    
    ## performanceMetrics: empty
    pf <- SimpleList(list("bench" = list()))

    SummarizedBenchmark(assays = a,
                        colData = df,
                        performanceMetrics = pf)
}

#' helper function to evaluate all quosures with data
eval_defaults <- function(b) {
        lapply(b$methods,
               function(x) {
                   expr <- quo(UQ(x$func)(!!! x$dparams))
                   if (is.function(eval_tidy(x$post, b$bdata))) {
                       expr <- quo(UQ(x$post)(!! expr))
                   }
                   eval_tidy(expr, b$bdata)
               })
}

#' helper function to convert method info to character for colData
clean_methods <- function(b) {
    DataFrame(blabel = names(b$methods),
              bfunc = sapply(b$methods,
                             function(x) { quo_name(x$func) }),
              bparams = sapply(b$methods,
                               function(x) {
                                   paste(names(x$dparams), "=",
                                         sapply(x$dparams, quo_text),
                                         collapse=", ") }),
              bpost = sapply(b$methods,
                             function(x)
                                 ifelse(is.function(eval_tidy(x$post, b$bdata)),
                                        quo_text(x$post), "NULL")))
}

#' Make benchmark results
#'
#' Function to evaluate BenchDesign to generate a
#' SummarizedBenchmark.
#' 
#' @param b BenchDesign object
#' 
#' @return
#' a SummarizedBenchmark with one assay.
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
    
    ## performanceFunctions: empty
    pf <- SimpleList(list("bench" = list()))

    SummarizedBenchmark(assays = a,
                        colData = df,
                        performanceFunctions = pf)
}

#' helper function to evaluate all the things
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

#' helper function to make method info easier to read
clean_methods <- function(b) {
    DataFrame(label = names(b$methods),
              func = sapply(b$methods,
                            function(x) { quo_name(x$func) }),
              params = sapply(b$methods,
                              function(x) {
                                  paste(names(x$dparams), "=",
                                        sapply(x$dparams, quo_text),
                                        collapse=", ") }),
              post = sapply(b$methods,
                            function(x)
                                ifelse(is.function(eval_tidy(x$post, b$bdata)),
                                       quo_text(x$post), "NULL")))
}

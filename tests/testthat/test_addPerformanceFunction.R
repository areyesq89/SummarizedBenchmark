context("performanceFunctions")
test_that("test='addPerformanceMetrics' returns correct error messages", {
  data(sb)
  fun <- function(query, truth, alpha=0.1){
    goodHits <- sum( (query < alpha) & truth == 1 )
    goodHits / sum(truth == 1)
  }
  expect_error( addPerformanceMetric(
    object=1:10,
    assay="qvalue",
    evalMetric="TPR",
    evalFunction=fun) )
  expect_error( addPerformanceMetric(
    sb,
    assay="notexisting",
    evalMetric="TPR",
    evalFunction=fun) )
  expect_error( addPerformanceMetric(
    sb,
    assay="notexisting",
    evalMetric=NULL,
    evalFunction=fun) )
  expect_error( addPerformanceMetric(
    sb,
    assay=NULL,
    evalMetric="TPR",
    evalFunction=fun) )
  expect_error( addPerformanceMetric(
    sb,
    assay="notexisting",
    evalMetric=c("TPR", "FDR"),
    evalFunction=fun) )
  expect_error( addPerformanceMetric(
    sb,
    assay="qvalue",
    evalFunction=fun) )
  fun <- function(query, truth2, alpha=0.1){
    goodHits <- sum( (query < alpha) & truth == 1 )
    goodHits / sum(truth == 1)
  }
  expect_error( addPerformanceMetric(
    sb,
    assay="qvalue",
    evalMetric="TPR",
    evalFunction=fun ) )
  fun <- function(query, truth, alpha){
    goodHits <- sum( (query < alpha) & truth == 1 )
    goodHits / sum(truth == 1)
  }
  expect_error( addPerformanceMetric(
    sb,
    assay="qvalue",
    evalMetric="TPR",
    evalFunction=fun ) )
} )

context("estimatePeformanceMetrics")
test_that("test='estimatePerformanceMetrics' returns informative messages", {
  data( sb )
  expect_error( estimatePerformanceMetrics( 1:10 ) )
  expect_error( estimatePerformanceMetrics( sb ) )
  sb <- addPerformanceMetric( sb, evalMetric=c("rejections", "TPR", "TNR", "FDR", "FNR"), assay="qvalue" )
  expect_s4_class( estimatePerformanceMetrics( sb ), "DataFrame" )
  tidyRes1 <- estimatePerformanceMetrics( sb, tidy=TRUE )
  expect_s3_class( tidyRes1, "data.frame" )
  sb <- estimatePerformanceMetrics( sb, addColData=TRUE )
  tidyRes2 <- tidyUpMetrics( sb )
  expect_identical( tidyRes1, tidyRes2 )
  expect_message( estimatePerformanceMetrics(sb) )
} )

test_that("test='estimatePerformanceMetrics' works with non scalar outputs", {
  data( sb )
  sb2 <- sb
  sb2 <- addPerformanceMetric(
    object=sb2,
    assay="qvalue",
    evalMetric="TPR",
    evalFunction = function( query, truth, alpha=0.1 ){
      goodHits <- sum( (query < alpha) & truth == 1 )
      goodHits / sum(truth == 1)
      c(goodHits, goodHits)
    }
  )
  expect_s4_class( estimatePerformanceMetrics(sb2, alpha=0.1 ), "DataFrame" )

  data( sb )
  sb2 <- sb
  sb2 <- addPerformanceMetric(
    object=sb2,
    assay="qvalue",
    evalMetric="TPR",
    evalFunction = function( query, truth, alpha=0.1 ){
      goodHits <- sum( (query < alpha) & truth == 1 )
      goodHits / sum(truth == 1)
      c(goodHits, goodHits)
    }
  )
  expect_s4_class( estimatePerformanceMetrics(sb2, alpha=c(0.1, 0.2) ), "DataFrame" )

  data( sb )
  sb2 <- sb
  sb2 <- addPerformanceMetric(
    object=sb2,
    assay="qvalue",
    evalMetric="TPR",
    evalFunction = function( query, truth ){
      goodHits <- sum(truth == 1)
      c(goodHits, goodHits, goodHits)
    }
  )
  expect_s4_class( estimatePerformanceMetrics(sb2, alpha=c(0.1, 0.2)), "DataFrame" )

  data( sb )
  sb2 <- sb
  sb2 <- addPerformanceMetric(
    object=sb2,
    assay="qvalue",
    evalMetric="TPR",
    evalFunction = function( query, truth, alpha=0.1 ){
      goodHits <- sum( (query < alpha) & truth == 1 )
      goodHits / sum(truth == 1)
    }
  )
  expect_s4_class( estimatePerformanceMetrics(sb2, alpha=c(0.1, 0.2)), "DataFrame" )

  data( sb )
  sb2 <- sb
  sb2 <- addPerformanceMetric(
    object=sb2,
    assay="qvalue",
    evalMetric="TPR",
    evalFunction = function( query, truth, alpha=0.1 ){
      goodHits <- sum( (query < alpha) & truth == 1 )
      goodHits / sum(truth == 1)
      c(SummarizedExperiment(), SummarizedExperiment())
    }
  )
  expect_s4_class( estimatePerformanceMetrics(sb2, alpha=c(0.1, 0.2)), "DataFrame" )
} )

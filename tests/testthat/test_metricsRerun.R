context("estimatePeformanceMetrics")
test_that("test='estimatePerformanceMetrics' with rerun=FALSE works appropiatetly", {
  df <- data.frame(pval = rnorm(100))
  bench <- BenchDesign(data = df)
  ## add methods
  bench <- addMethod(bench, label = "bonf", func = p.adjust,
                   params = rlang::quos(p = pval, method = "bonferroni"))
  bench <- addMethod(bench, label = "BH", func = p.adjust,
                   params = rlang::quos(p = pval, method = "BH"))
  ## evaluate benchmark experiment
  sb <- buildBench(bench)
  ## evaluate benchmark experiment w/ data sepecified
  sb <- buildBench(bench, data = df)
  sb <- addPerformanceMetric( sb, assay="default", evalMetric="rejections" )

  ## calling it twice throws an error saying it is up to date ##
  expect_error( estimatePerformanceMetrics(
    estimatePerformanceMetrics(sb, addColData=TRUE, rerun=FALSE ),
    addColData=TRUE, rerun=FALSE ) )

  ## results are identical between rerun=TRUE and rerun=FALSE ##
  sb <- estimatePerformanceMetrics(sb, addColData=TRUE)
  bench <- addMethod( bench, label="BH2", func=p.adjust,
             params = rlang::quos(p = pval, method = "BH") )
  sb2 <- updateBench( sb, bd=bench, dryrun=FALSE )
  res1 <- colData( estimatePerformanceMetrics( sb2, rerun=FALSE, addColData=TRUE ) )
  sb2 <- estimatePerformanceMetrics( sb2, rerun=TRUE, addColData=TRUE )
  res2 <- colData( sb2 )
  expect_identical( res2[,colnames( res1 )], res1 )

  ## Also works when modifying a method ##
  bench <- modifyMethod( bench,
                       label="BH",
                       param=rlang::quos(p=pval, method="hommel"))
  sb2 <- updateBench( sb2, bd=bench, dryrun=FALSE )
  expect_s4_class( estimatePerformanceMetrics( sb2, rerun=FALSE ), "DataFrame")
})

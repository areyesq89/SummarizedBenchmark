context("SummarizedBenchmark")
test_that("test='SummarizedBenchmark' returns error messages", {
  library(iCOBRA)
  data(cobradata_example)
  assays <- list(
    qvalue=cobradata_example@padj,
    logFC=cobradata_example@score )
  assays[["qvalue"]]$DESeq2 <- p.adjust(cobradata_example@pval$DESeq2, method="BH")
  groundTruth <- DataFrame( cobradata_example@truth[,c("status", "logFC")] )
  colnames(groundTruth) <- names( assays )
  colData <- DataFrame( method=colnames(assays[[1]]) )
  groundTruth <- groundTruth[rownames(assays[[1]]),]
  expect_error( SummarizedBenchmark(
    assays=assays,
    colData=colData,
    groundTruth=groundTruth[1:10,] ) )
  assays2 <- assays
  assays2[[1]] <- assays2[[1]][1:10,]
  expect_error( SummarizedBenchmark(
    assays=assays2,
    colData=colData,
    groundTruth=groundTruth ) )
  assays2 <- assays
  assays2[[1]] <- assays2[[1]][,1:2]
  expect_error( SummarizedBenchmark(
    assays=assays2,
    colData=colData,
    groundTruth=groundTruth ) )
} )

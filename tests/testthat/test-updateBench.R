library(SummarizedBenchmark)
context("updateBench")

## sample t-test data set
data(tdat)

test_that("basic updateBench call works", {
    ## basic BenchDesign
    bd <- BenchDesign(data = tdat)
    BDMethod(bd, "bonf") <- BDMethod(x = p.adjust,
                                     params = rlang::quos(p = pval, method = "bonferroni"),
                                     meta = list(purpose = "for testing"))
    BDMethod(bd, "bh") <- BDMethod(x = p.adjust,
                                   params = rlang::quos(p = pval, method = "BH"),
                                   meta = list(purpose = "for comparing"))
    sb <- buildBench(bd)

    ## basic call w/ just SummarizedBenchmark
    expect_silent(bdcomp <- compareBenchDesigns(sb))
    expect_output(bdupdate <- updateBench(sb), "Update SummarizedBenchmark \\(dryrun\\)")
    expect_equal(bdcomp$methods$res, bdupdate$methods$res)
    expect_equal(dplyr::select(bdcomp$methods[[2]], -post, -f),
                 dplyr::select(bdupdate$methods[[2]], -post, -f))
    expect_equal(dplyr::select(bdcomp$methods[[3]], -post, -f),
                 dplyr::select(bdupdate$methods[[3]], -post, -f))
    expect_equal(bdcomp$data, bdupdate$data)
    expect_equal(bdcomp$methods$res$overlap, rep("Both", 2))
    expect_true(all(unlist(dplyr::select_if(bdcomp$methods$res, is.logical))))

    expect_error(sb2 <- updateBench(sb, dryrun = FALSE), "MD5 hash")
    expect_silent(sb2 <- updateBench(sb, dryrun = FALSE, data = tdat))
    expect_identical(sb, sb2)
    
    ## basic call w/ new BenchDesign
    bd2 <- bd
    BDMethod(bd2, "holm") <- BDMethod(x = p.adjust, params = rlang::quos(p = pval, method = "holm"))
    BDMethod(bd2, "bh") <- NULL

    expect_silent(bdcomp <- compareBenchDesigns(sb, bd2))
    expect_output(bdupdate <- updateBench(sb, bd2), "Update SummarizedBenchmark \\(dryrun\\)")
    expect_output(updateBench(sb, bd2, keepAll = FALSE), "Drop")
    expect_equal(bdcomp$methods$res, bdupdate$methods$res)
    expect_equal(dplyr::filter(bdupdate$methods$res, label == "holm")$overlap, "yOnly")
    expect_equal(dplyr::filter(bdupdate$methods$res, label == "bh")$overlap, "xOnly")
    expect_equal(dplyr::filter(bdupdate$methods$res, label == "bonf")$overlap, "Both")
    expect_true(all(is.na(unlist(dplyr::select_if(dplyr::filter(bdcomp$methods$res, label == "holm"), is.logical)))))

    expect_silent(sb2 <- updateBench(sb, bd2, dryrun = FALSE, data = tdat))
    expect_silent(sb2drop <- updateBench(sb, bd2, dryrun = FALSE, keepAll = FALSE, data = tdat))

    expect_equal(ncol(sb2), 3L)
    expect_equal(ncol(sb2drop), 2L)
    expect_equal(sort(colnames(sb2)), c("bh", "bonf", "holm"))
    expect_equal(sort(colnames(sb2drop)), c("bonf", "holm"))

    expect_length(BDMethodList(sb2), 3L)
    expect_length(BDMethodList(sb2drop), 2L)

    expect_equal(length(metadata(sb2)$sessions), 2L)
    expect_equal(length(metadata(sb2drop)$sessions), 2L)

    expect_equal(sort(metadata(sb2)$sessions[[1]]$methods), c("bh", "bonf"))
    expect_equal(metadata(sb2)$sessions[[2]]$methods, "holm")

    expect_equal(metadata(sb2drop)$sessions[[1]]$methods, "bonf")
    expect_equal(metadata(sb2drop)$sessions[[2]]$methods, "holm")
    expect_equal(names(metadata(sb2drop)$sessions[[1]]$results), "bonf")
    expect_equal(names(metadata(sb2drop)$sessions[[2]]$results), "holm")
})

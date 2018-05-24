library(SummarizedBenchmark)
library(BiocParallel)
context("benchDesign")

## sample t-test data set
data(tdat)


test_that("basic buildBench call works", {
    ## basic BenchDesign
    bd <- BenchDesign(data = tdat)
    bd <- addMethod(bd,
                    label = "bonf",
                    func = p.adjust,
                    params = rlang::quos(p = pval, method = "bonferroni"),
                    meta = list(purpose = "for testing"))
    bd <- addMethod(bd,
                    label = "bh",
                    func = p.adjust,
                    params = rlang::quos(p = pval, method = "BH"),
                    meta = list(purpose = "for comparing"))

    ## check simplest call returns SummarizedBenchmark with an assay
    expect_silent(sb <- buildBench(bd))
    expect_is(sb, "SummarizedBenchmark")
    expect_equal(colnames(sb), c("bonf", "bh"))
    expect_length(assays(sb), 1)
    ## check values against direct call of p.adjust
    expect_equal(assays(sb)[["default"]][, "bonf"],
                 p.adjust(tdat$pval, "bonferroni"))
    expect_equal(assays(sb)[["default"]][, "bh"],
                 p.adjust(tdat$pval, "BH"))
    
    ## check truthCols, ftCols specification
    sb_cols <- buildBench(bd, truthCols = "H", ftCols = c("effect_size", "SE"))
    ## check name of assay is now truthCols
    expect_equal(names(assays(sb_cols)), "H")
    ## check rowData now populated
    expect_equal(sort(c("H", "effect_size", "SE")),
                 sort(names(rowData(sb_cols))))
    ## check values actually correct
    expect_equal(rowData(sb_cols)[, "H"], tdat$H)
    expect_equal(rowData(sb_cols)[, "SE"], tdat$SE)

    ## empty BenchDesign
    bd_empty <- BenchDesign()
    bd_empty <- addMethod(bd_empty,
                          label = "bonf",
                          func = p.adjust,
                          params = rlang::quos(p = pval, method = "bonferroni"),
                          meta = list(purpose = "for testing"))
    bd_empty <- addMethod(bd_empty,
                          label = "bh",
                          func = p.adjust,
                          params = rlang::quos(p = pval, method = "BH"),
                          meta = list(purpose = "for comparing"))

    ## check call with data specified at buildBench step
    expect_silent(sb_empty <- buildBench(bd_empty, data = tdat))
    expect_equal(sb, sb_empty)

    ## BenchDesign with keyword metadata
    bd_kw <- BenchDesign(data = tdat)
    bd_kw <- addMethod(bd_kw,
                       label = "bonf",
                       func = p.adjust,
                       params = rlang::quos(p = pval, method = "bonferroni"),
                       meta = list(pkg_func = rlang::quo(testthat::expect_equal)))
    bd_kw <- addMethod(bd_kw,
                       label = "bh",
                       func = p.adjust,
                       params = rlang::quos(p = pval, method = "BH"),
                       meta = list(purpose = "for comparing",
                                   pkg_vers = "100", pkg_name = "nothing"))

    expect_silent(sb_kw <- buildBench(bd_kw))
    expect_equal(colData(sb_kw)$func.pkg, c("testthat", "nothing"))
    expect_equal(colData(sb_kw)$func.pkg.vers, c(as.character(packageVersion("testthat")),
                                                 "100"))
})


test_that("multi-assay BenchDesigns are handled", {
    ## multi-assay BenchDesign
    bd <- BenchDesign(data = tdat)
    bd <- addMethod(bd,
                    label = "bonf",
                    func = p.adjust,
                    params = rlang::quos(p = pval, method = "bonferroni"),
                    post = list(a1 = function(x) { x * 1 },
                                a2 = function(x) { x * 2 }),
                    meta = list(purpose = "for testing"))
    bd <- addMethod(bd,
                    label = "bh",
                    func = p.adjust,
                    params = rlang::quos(p = pval, method = "BH"),
                    post = list(a1 = function(x) { x },
                                a2 = function(x) { x / 2 }),
                    meta = list(purpose = "for comparing"))

    ## check returns SummarizedBenchmark with 2 assays
    expect_silent(sb <- buildBench(bd, truthCols = c(a1 = "H", a2 = "effect_size")))
    expect_equal(colnames(sb), c("bonf", "bh"))
    expect_equal(names(assays(sb)), c("a1", "a2"))
    ## check post functions evaluated as defined
    expect_equal(assay(sb, "a1")[, "bonf"] * 2,
                 assay(sb, "a2")[, "bonf"])
    expect_equal(assay(sb, "a1")[, "bh"] / 2,
                 assay(sb, "a2")[, "bh"])
    ## check truthCols have names matching assays, values matching original data
    expect_equal(rowData(sb)[, "a1"], tdat$H)
    expect_equal(rowData(sb)[, "a2"], tdat$effect_size)
})


test_that("parallelization is accepted", {
    ## multi-assay BenchDesign
    bd <- BenchDesign(data = tdat)
    bd <- addMethod(bd,
                    label = "bonf",
                    func = p.adjust,
                    params = rlang::quos(p = pval, method = "bonferroni"),
                    post = list(a1 = function(x) { x * 1 },
                                a2 = function(x) { x * 2 }),
                    meta = list(purpose = "for testing"))
    bd <- addMethod(bd,
                    label = "bh",
                    func = p.adjust,
                    params = rlang::quos(p = pval, method = "BH"),
                    post = list(a1 = function(x) { x },
                                a2 = function(x) { x / 2 }),
                    meta = list(purpose = "for comparing"))

    ## compare output run with and without parallelization
    sb <- buildBench(bd, parallel = FALSE)
    expect_silent(sb_par <- buildBench(bd, parallel = TRUE, BPPARAM = SerialParam()))
    ## check that everything except session metadata is the same
    expect_false(isTRUE(all.equal(metadata(sb)$sessions[[1]]$parameters,
                                  metadata(sb_par)$sessions[[1]]$parameters)))
    metadata(sb)$sessions[[1]]$parameters <- NULL
    metadata(sb_par)$sessions[[1]]$parameters <- NULL
    expect_equal(sb, sb_par)
})


test_that("errors thrown with inappropriate inputs", {
    ## check error if BenchDesign has no data, no methods
    expect_error(sb <- buildBench(BenchDesign()),
                 "data in BenchDesign is NULL")

    ## check error if BenchDesign has no data, but methods
    expect_error(sb <- buildBench(BenchDesign(data = tdat)),
                 "list of methods in BenchDesign is empty")

    ## check proper handling if post only list for one method
    bd <- addMethod(BenchDesign(data = tdat), label = "bonf",
                    func = p.adjust, params = rlang::quos(p = pval, method = "bonferroni"),
                    post = list(a1 = function(x) { x * 1 }))
    bd <- addMethod(bd, label = "bh",
                    func = p.adjust, params = rlang::quos(p = pval, method = "BH"),
                    post = function(x) { x * 1 })
    expect_silent(sb <- buildBench(bd))
    expect_equal(assayNames(sb), "a1")

    ## check proper handling if inconsistent length across methods
    bd <- addMethod(BenchDesign(data = tdat), label = "bonf",
                    func = p.adjust, params = rlang::quos(p = pval, method = "bonferroni"),
                    post = list(a1 = function(x) { x * 1 }, a2 = function(x) { x * 2 }))
    bd <- addMethod(bd, label = "bonf2",
                    func = p.adjust, params = rlang::quos(p = pval, method = "bonferroni"),
                    post = list(a1 = function(x) { x }))
    expect_silent(sb <- buildBench(bd))
    expect_true(all(is.na(assay(sb, "a2")[, "bonf2"])))
    expect_equal(assay(sb, "a1")[, "bonf"] * 2, assay(sb, "a2")[, "bonf"])
    expect_equal(assay(sb, "a1")[, "bonf"], assay(sb, "a1")[, "bonf2"])

    ## check proper handling if truthCols unnamed when post specified as list 
    bd <- addMethod(BenchDesign(data = tdat), label = "bonf",
                    func = p.adjust, params = rlang::quos(p = pval, method = "bonferroni"),
                    post = list(a1 = function(x) { x * 1 }))
    bd <- addMethod(bd, label = "bh",
                    func = p.adjust, params = rlang::quos(p = pval, method = "BH"),
                    post = list(a1 = function(x) { x }))
    expect_silent(buildBench(bd, truthCols = "H"))

    ## check error if truthCols not column in original data
    bd <- addMethod(BenchDesign(data = tdat), label = "bonf", func = p.adjust,
                    params = rlang::quos(p = pval, method = "bonferroni"))
    bd <- addMethod(bd, label = "bh", func = p.adjust,
                    params = rlang::quos(p = pval, method = "BH"))
    expect_error(buildBench(bd, truthCols = "apple"))
    
    ## check error if invalid ftCols is specified
    expect_error(buildBench(bd, ftCols = "apple"), "Invalid ftCols specification")
})


test_that("buildBench can handle sortIDs", {
    ## simple data set with index rows
    newdat <- list(myids = rev(letters[1:5]),
                   mytru = 1:5)
    
    ## basic BenchDesign w/ 1 assay, different length output
    bd1 <- BenchDesign(data = newdat)
    bd1 <- addMethod(bd1, label = "abc",
                     func = function() { c(a = 1, b = 2, c = 3) })
    bd1 <- addMethod(bd1, label = "bcef",
                     func = function() { c(b = 11, c = 12, e = 13, f = 15) })
    ## check behavior when output has unequal lengths
    expect_warning(buildBench(bd1), "different lengths")

    ## check behavior when sorting without ID column
    sb1a <- buildBench(bd1, sortIDs = TRUE)
    expect_is(sb1a, "SummarizedBenchmark")
    expect_equal(rownames(sb1a), c("a", "b", "c", "e", "f"))
    expect_true(is.na(assay(sb1a)["a", "bcef"]))
    expect_error(buildBench(bd1, truthCols = "mytru", sortIDs = TRUE))

    ## check behavior when sorting with ID column
    sb1b <- buildBench(bd1, sortIDs = "myids")
    expect_is(sb1b, "SummarizedBenchmark")
    expect_equal(rownames(assay(sb1b)), newdat$myids)
    expect_true(is.na(assay(sb1b)["a", "bcef"]))


    ## basic BenchDesign w/ 2 assays, different length output
    bd2 <- BenchDesign(data = newdat)
    bd2 <- addMethod(bd2, label = "abc",
                     func = function() { c(a = 1, b = 2, c = 3) },
                     post = list(o1 = function(x) { x },
                                 o2 = function(x) { x*2 }))
    bd2 <- addMethod(bd2, label = "bcef",
                     func = function() { c(b = 11, c = 12, e = 13, f = 15) },
                     post = list(o1 = function(x) { x },
                                 o2 = function(x) { x*2[1:2] }))

    ## check behavior when output has unequal lengths
    expect_warning(buildBench(bd2))

    ## check behavior when sorting without ID column
    sb2a <- buildBench(bd2, sortIDs = TRUE)
    expect_is(sb2a, "SummarizedBenchmark")
    expect_length(assays(sb2a), 2)
    expect_equal(rownames(sb2a), c("a", "b", "c", "e", "f"))
    expect_true(is.na(assay(sb2a, "o2")["a", "bcef"]))
    expect_error(buildBench(bd2, truthCols = "mytru", sortIDs = TRUE))

    ## check behavior when sorting with ID column
    sb2b <- buildBench(bd2, sortIDs = "myids")
    expect_is(sb2b, "SummarizedBenchmark")
    expect_equal(rownames(sb2b), newdat$myids)
    expect_true(is.na(assay(sb2b, "o2")["a", "bcef"]))
})

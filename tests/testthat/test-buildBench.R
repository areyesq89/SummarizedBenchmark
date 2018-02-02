library(SummarizedBenchmark)
library(BiocParallel)
context("benchDesign")

## sample t-test data set
data(tdat)


test_that("basic buildBench call works", {
    ## basic BenchDesign
    bd <- BenchDesign(tdat)
    bd <- addBMethod(bd,
                     blabel = "bonf",
                     bfunc = p.adjust,
                     p = pval, method = "bonferroni",
                     bmeta = list(purpose = "for testing"))
    bd <- addBMethod(bd,
                     blabel = "bh",
                     bfunc = p.adjust,
                     p = pval, method = "BH",
                     bmeta = list(purpose = "for comparing"))

    ## check simplest call returns SummarizedBenchmark with an assay
    expect_silent(sb <- buildBench(bd))
    expect_is(sb, "SummarizedBenchmark")
    expect_equal(colnames(sb), c("bonf", "bh"))
    expect_length(assays(sb), 1)
    ## check values against direct call of p.adjust
    expect_equal(assays(sb)[["bench"]][, "bonf"],
                 p.adjust(tdat$pval, "bonferroni"))
    expect_equal(assays(sb)[["bench"]][, "bh"],
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
    bd_empty <- addBMethod(bd_empty,
                           blabel = "bonf",
                           bfunc = p.adjust,
                           p = pval, method = "bonferroni",
                           bmeta = list(purpose = "for testing"))
    bd_empty <- addBMethod(bd_empty,
                           blabel = "bh",
                           bfunc = p.adjust,
                           p = pval, method = "BH",
                           bmeta = list(purpose = "for comparing"))

    ## check call with data specified at buildBench step
    expect_silent(sb_empty <- buildBench(bd_empty, data = tdat))
    expect_equal(sb, sb_empty)

    ## BenchDesign with keyword metadata
    devtools::load_all()
    bd_kw <- BenchDesign(tdat)
    bd_kw <- addBMethod(bd_kw,
                        blabel = "bonf",
                        bfunc = p.adjust,
                        p = pval, method = "bonferroni",
                        bmeta = list(pkg_func = rlang::quo(testthat::expect_equal)))
    bd_kw <- addBMethod(bd_kw,
                        blabel = "bh",
                        bfunc = p.adjust,
                        p = pval, method = "BH",
                        bmeta = list(purpose = "for comparing",
                                     pkg_vers = "100", pkg_name = "nothing"))

    expect_silent(sb_kw <- buildBench(bd_kw))
    expect_equal(colData(outp)$pkg_name, c("testthat", "nothing"))
    expect_equal(colData(outp)$pkg_vers, c(as.character(packageVersion("testthat")),
                                           "100"))
})


test_that("multi-assay BenchDesigns are handled", {
    ## multi-assay BenchDesign
    bd <- BenchDesign(tdat)
    bd <- addBMethod(bd,
                     blabel = "bonf",
                     bfunc = p.adjust,
                     p = pval, method = "bonferroni",
                     bpost = list(a1 = function(x) { x * 1 },
                                  a2 = function(x) { x * 2 }),
                     bmeta = list(purpose = "for testing"))
    bd <- addBMethod(bd,
                     blabel = "bh",
                     bfunc = p.adjust,
                     p = pval, method = "BH",
                     bpost = list(a1 = function(x) { x },
                                  a2 = function(x) { x / 2 }),
                     bmeta = list(purpose = "for comparing"))

    ## check returns SummarizedBenchmark with 2 assays
    expect_silent(sb <- buildBench(bd, truthCols = c(a1 = "H", a2 = "effect_size")))
    expect_equal(colnames(sb), c("bonf", "bh"))
    expect_equal(names(assays(sb)), c("a1", "a2"))
    ## check bpost functions evaluated as defined
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
    bd <- BenchDesign(tdat)
    bd <- addBMethod(bd,
                     blabel = "bonf",
                     bfunc = p.adjust,
                     p = pval, method = "bonferroni",
                     bpost = list(a1 = function(x) { x * 1 },
                                  a2 = function(x) { x * 2 }),
                     bmeta = list(purpose = "for testing"))
    bd <- addBMethod(bd,
                     blabel = "bh",
                     bfunc = p.adjust,
                     p = pval, method = "BH",
                     bpost = list(a1 = function(x) { x },
                                  a2 = function(x) { x / 2 }),
                     bmeta = list(purpose = "for comparing"))

    ## compare output run with and without parallelization
    sb <- buildBench(bd, parallel = FALSE)
    expect_silent(sb_par <- buildBench(bd, parallel = TRUE, BPPARAM = SerialParam()))
    expect_equal(sb, sb_par)
})


test_that("errors thrown with inappropriate inputs", {
    ## check error if BenchDesign has no data, no methods
    expect_error(sb <- buildBench(BenchDesign()),
                 "data in BenchDesign is NULL")

    ## check error if BenchDesign has no data, but methods
    expect_error(sb <- buildBench(BenchDesign(tdat)),
                 "list of methods in BenchDesign is empty")

    ## check error if bpost only list for one method
    bd <- addBMethod(BenchDesign(tdat), blabel = "bonf",
                     bfunc = p.adjust, p = pval, method = "bonferroni",
                     bpost = list(a1 = function(x) { x * 1 }))
    bd <- addBMethod(bd, blabel = "bh",
                     bfunc = p.adjust, p = pval, method = "BH",
                     bpost = function(x) { x * 1 })
    expect_error(buildBench(bd), "Inconsistent bpost specification style across methods.")

    ## check error in bpost specification if inconsistent length across methods
    bd <- addBMethod(BenchDesign(tdat), blabel = "bonf",
                     bfunc = p.adjust, p = pval, method = "bonferroni",
                     bpost = list(a1 = function(x) { x * 1 }, a2 = function(x) { x * 2 }))
    bd <- addBMethod(bd, blabel = "bh",
                     bfunc = p.adjust, p = pval, method = "BH",
                     bpost = list(a1 = function(x) { x }))
    expect_error(buildBench(bd), "Inconsistent bpost length across methods.")

    ## check error in bpost specification if inconsistent naming across methods
    bd <- addBMethod(BenchDesign(tdat), blabel = "bonf",
                     bfunc = p.adjust, p = pval, method = "bonferroni",
                     bpost = list(a1 = function(x) { x * 1 }, a2 = function(x) { x * 2 }))
    bd <- addBMethod(bd, blabel = "bh",
                     bfunc = p.adjust, p = pval, method = "BH",
                     bpost = list(a100 = function(x) { x }, a200 = function(x) { x * 2 }))
    expect_error(buildBench(bd), "Inconsistent bpost naming across methods.")

    ## check error if truthCols not column in original data
    bd <- addBMethod(BenchDesign(tdat), blabel = "bonf", bfunc = p.adjust, p = pval, method = "bonferroni")
    bd <- addBMethod(bd, blabel = "bh", bfunc = p.adjust, p = pval, method = "BH")
    expect_error(buildBench(bd, truthCols = "apple"))
    
    ## check error if truthCols names don't match assay names when bpost specified as list 
    bd <- addBMethod(BenchDesign(tdat), blabel = "bonf",
                     bfunc = p.adjust, p = pval, method = "bonferroni",
                     bpost = list(a1 = function(x) { x * 1 }))
    bd <- addBMethod(bd, blabel = "bh",
                     bfunc = p.adjust, p = pval, method = "BH",
                     bpost = list(a1 = function(x) { x }))
    expect_error(buildBench(bd, truthCols = "H"))

    ## check error if invalid ftCols is specified
    expect_error(buildBench(bd, ftCols = "apple"), "Invalid ftCols specification")
})


test_that("buildBench can handle sortIDs", {
    ## simple data set with index rows
    newdat <- list(myids = rev(letters[1:5]),
                   mytru = 1:5)
    
    ## basic BenchDesign w/ 1 assay, different length output
    bd1 <- BenchDesign(newdat)
    bd1 <- addBMethod(bd1, blabel = "abc",
                      bfunc = function() { c(a = 1, b = 2, c = 3) })
    bd1 <- addBMethod(bd1, blabel = "bcef",
                      bfunc = function() { c(b = 11, c = 12, e = 13, f = 15) })
    
    ## check behavior when output has unequal lengths
    expect_error(buildBench(bd1))

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
    bd2 <- BenchDesign(newdat)
    bd2 <- addBMethod(bd2, blabel = "abc",
                      bfunc = function() { c(a = 1, b = 2, c = 3) },
                      bpost = list(o1 = function(x) { x },
                                   o2 = function(x) { x*2 }))
    bd2 <- addBMethod(bd2, blabel = "bcef",
                      bfunc = function() { c(b = 11, c = 12, e = 13, f = 15) },
                      bpost = list(o1 = function(x) { x },
                                   o2 = function(x) { x*2 }))

    ## check behavior when output has unequal lengths
    expect_error(buildBench(bd2))

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

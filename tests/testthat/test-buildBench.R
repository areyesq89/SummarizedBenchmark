library(SummarizedBenchmark)
library(BiocParallel)
context("BenchDesign")

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


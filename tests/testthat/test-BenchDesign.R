library(SummarizedBenchmark)
context("BenchDesign")

## sample t-test data set
data(tdat)


test_that("constructor accepts empty input", {
    ## check BenchDesign object returned 
    expect_silent(bd <- BenchDesign())
    expect_true(is.BenchDesign(bd))
    expect_is(bd, "BenchDesign")

    ## check print call with NULL data
    expect_output(print(bd), "NULL")

    ## check print call with no methods
    expect_output(print(bd), "none")
})


test_that("constructor accepts data input", {
    ## check BenchDesign object returned 
    expect_silent(bd <- BenchDesign(tdat))
    expect_true(is.BenchDesign(bd))

    ## check data in object same as original data
    expect_equal(bd$bdata, tdat)
})


test_that("methods can be added and removed", {
    bd <- BenchDesign(tdat)

    bd <- addMethod(bd,
                    label = "bonf",
                    func = p.adjust,
                    p = pval, method = "bonferroni",
                    meta = list(purpose = "for testing"))

    ## check returned object
    expect_is(bd, "BenchDesign")
    expect_length(bd$methods, 1)
    expect_equal(names(bd$methods)[1], "bonf")

    ## check print call with method
    expect_output(print(bd), "BenchDesign object.*bonf")

    ## check printMethod, printMethods print
    expect_output(printMethod(bd, "bonf"), "bonf")
    expect_output(printMethods(bd), "bonf")

    ## check method can be removed
    expect_silent(bd_drop <- dropMethod(bd, "bonf"))

    ## check error when trying to remove undefined method 
    expect_error(dropMethod(bd, "apple"), "method is not defined")
})


test_that("methods can be modified", {
    bd <- BenchDesign(tdat)
    bd <- addMethod(bd,
                    label = "bonf",
                    func = p.adjust,
                    p = pval, method = "bonferroni")

    ## check basic method modification
    bd_mod <- modifyMethod(bd, "bonf", p = pval / 2)
    ## check only single method, but with new param value
    expect_length(bd_mod$methods, 1)
    expect_equal(bd_mod$methods$bonf$dparams$p, quo(pval / 2))
    expect_equal(bd_mod$methods$bonf$dparams$method, quo("bonferroni"))
    
    ## check method modification with overwrite
    bd_ow <- modifyMethod(bd, "bonf", p = pval / 2,
                          .overwrite = TRUE)
    ## check only single method, but with only new param value
    expect_length(bd_ow$methods, 1)
    expect_equal(bd_ow$methods$bonf$dparams$p, quo(pval / 2))
    expect_null(bd_ow$methods$bonf$dparams$method)

    ## check handling of non-parameter "special" values (func, post, meta)
    bd_spec <- modifyMethod(bd, "bonf",
                            func = function(x) { x }, post = p.adjust,
                            meta = list(new_purpose = "test special values"))
    expect_equal(names(bd_spec$methods), "bonf")
    expect_equal(bd_spec$methods$bonf$func, quo(function(x) { x }))
    expect_equal(bd_spec$methods$bonf$post, quo(p.adjust))
    expect_equal(bd_spec$methods$bonf$meta, list(new_purpose = "test special values"))

    ## check error when invalid method specified
    expect_error(modifyMethod(bd, "apple", p = pval / 2), "not defined")
})


test_that("methods can be expanded", {
    bd <- BenchDesign(tdat)
    bd <- addMethod(bd,
                    label = "bonf",
                    func = p.adjust,
                    p = pval, method = "bonferroni")

    ## check basic method expansion
    bd_exp <- expandMethod(bd, "bonf", param = "p",
                           bonf_alt1 = pval / 2,
                           bonf_alt2 = pval / 4)
    expect_equal(names(bd_exp$methods), c("bonf", "bonf_alt1", "bonf_alt2"))
    ## check expanded param changed (note: values equal but not identical)
    expect_equal(bd_exp$methods$bonf_alt1$dparams$p, quo(pval / 2))
    expect_equal(bd_exp$methods$bonf_alt2$dparams$p, quo(pval / 4))
    ## check other param unchanged
    expect_identical(bd_exp$methods$bonf$dparams$method,
                     bd_exp$methods$bonf_alt2$dparams$method)
    ## check defined methods are valid and don't break buildBench call
    expect_is(buildBench(bd_exp), "SummarizedBenchmark")
    
    ## check method expansion w/ replacing original method
    bd_replace <- expandMethod(bd, "bonf", param = "p",
                               bonf_alt1 = pval / 2,
                               bonf_alt2 = pval / 4,
                               .replace = TRUE)
    expect_equal(names(bd_replace$methods), c("bonf_alt1", "bonf_alt2"))
    ## check defined methods are valid and don't break buildBench call
    expect_is(buildBench(bd_replace), "SummarizedBenchmark")

    ## check method expansion w/ mult params
    bd_mult <- expandMethod(bd, "bonf",
                            bonf_alt1 = list(p = pval / 2),
                            bonf_alt2 = list(p = pval / 4, method = "BH"))
    expect_equal(names(bd_mult$methods), c("bonf", "bonf_alt1", "bonf_alt2"))
    ## check params changed only for expected (note: values equal but not identical)
    expect_equal(bd_mult$methods$bonf_alt1$dparams$p, quo(pval / 2))
    expect_equal(bd_mult$methods$bonf_alt1$dparams$method, quo("bonferroni"))
    expect_equal(bd_mult$methods$bonf_alt2$dparams$p, quo(pval / 4))
    expect_equal(bd_mult$methods$bonf_alt2$dparams$method, quo("BH"))
    ## check defined methods are valid and don't break buildBench call
    expect_is(buildBench(bd_mult), "SummarizedBenchmark")

    ## check method expansion w/ mult params and overwriting all 'params' 
    bd_ow <- expandMethod(bd, "bonf",
                          bonf_alt1 = list(p = pval / 2),
                          bonf_alt2 = list(p = pval / 4, method = "BH"),
                          .overwrite = TRUE)
    expect_equal(names(bd_ow$methods), c("bonf", "bonf_alt1", "bonf_alt2"))
    ## check params changed only for expected (note: values equal but not identical)
    expect_equal(bd_ow$methods$bonf_alt1$dparams$p, quo(pval / 2))
    expect_null(bd_ow$methods$bonf_alt1$dparams$method)
    expect_equal(bd_ow$methods$bonf_alt2$dparams$p, quo(pval / 4))
    expect_equal(bd_ow$methods$bonf_alt2$dparams$method, quo("BH"))
    ## check defined methods are valid and don't break buildBench call
    expect_is(buildBench(bd_ow), "SummarizedBenchmark")

    ## check handling of non-parameter "special" values (func, post, meta)
    bd_spec <- expandMethod(bd, "bonf",
                            bonf_alt1 = list(func = function(p, method) { p },
                                             post = p.adjust,
                                             meta = list(new_purpose = "test special values")),
                            bonf_alt2 = list(func = function(p, method) { p / 2 },
                                             post = function(x) { x * 2 },
                                             meta = list(new_purpose = "test special values again")),
                            .replace = TRUE)
    expect_equal(names(bd_spec$methods), c("bonf_alt1", "bonf_alt2"))
    expect_equal(bd_spec$methods$bonf_alt1$func, quo(function(p, method) { p }))
    expect_equal(bd_spec$methods$bonf_alt1$post, quo(p.adjust))
    expect_equal(bd_spec$methods$bonf_alt1$meta, list(new_purpose = "test special values"))
    expect_equal(bd_spec$methods$bonf_alt2$func, quo(function(p, method) { p / 2 }))
    expect_equal(bd_spec$methods$bonf_alt2$post, quo(function(x) { x * 2}))
    expect_equal(bd_spec$methods$bonf_alt2$meta, list(new_purpose = "test special values again"))
    ## check defined methods are valid and don't break buildBench call
    expect_is(buildBench(bd_spec), "SummarizedBenchmark")
    
    ## check error when invalid method specified
    expect_error(expandMethod(bd, "apple", bonf_alt1 = list(p = pval / 2)), "not defined")

    ## check error when name conflicts introduced
    expect_error(expandMethod(bd, "bonf", bonf = list(p = pval / 2)), "should not overlap")
    expect_error(expandMethod(bd, "bonf", param = NULL, list(p = pval / 2)), "must be named")
    expect_error(expandMethod(bd, "bonf",
                              bonf_alt = list(p = pval / 2),
                              bonf_alt = list(p = pval / 4)), "must be unique")
})

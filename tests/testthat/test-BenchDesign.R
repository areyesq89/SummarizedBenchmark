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
                    params = rlang::quos(p = pval, method = "bonferroni"),
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
                    params = rlang::quos(p = pval, method = "bonferroni"))

    ## check basic method modification
    bd_mod <- modifyMethod(bd, "bonf", params = rlang::quos(p = pval / 2))
    ## check only single method, but with new param value
    expect_length(bd_mod$methods, 1)
    expect_equal(bd_mod$methods$bonf$params$p, rlang::quo(pval / 2))
    expect_equal(bd_mod$methods$bonf$params$method, rlang::quo("bonferroni"))
    
    ## check method modification with overwrite
    bd_ow <- modifyMethod(bd, "bonf", params = rlang::quos(p = pval / 2),
                          .overwrite = TRUE)
    ## check only single method, but with only new param value
    expect_length(bd_ow$methods, 1)
    expect_equal(bd_ow$methods$bonf$params$p, rlang::quo(pval / 2))
    expect_null(bd_ow$methods$bonf$params$method)

    ## check handling of non-parameter "special" values (func, post, meta)
    bd_spec <- modifyMethod(bd, "bonf",
                            params = rlang::quos(bd.func = function(x) { x },
                                                 bd.post = p.adjust,
                                                 bd.meta = list(new_purpose = "test special values")))
    expect_equal(names(bd_spec$methods), "bonf")
    expect_equal(bd_spec$methods$bonf$func, rlang::quo(function(x) { x }))
    expect_equal(bd_spec$methods$bonf$post, rlang::quo(p.adjust))
    expect_equal(bd_spec$methods$bonf$meta, list(new_purpose = "test special values"))

    ## check error when invalid method specified
    expect_error(modifyMethod(bd, "apple", params = rlang::quos(p = pval / 2)),
                 "not defined")
})


test_that("methods can be expanded", {
    bd <- BenchDesign(tdat)
    bd <- addMethod(bd,
                    label = "bonf",
                    func = p.adjust,
                    params = rlang::quos(p = pval, method = "bonferroni"))

    ## check basic method expansion
    bd_exp <- expandMethod(bd, "bonf", onlyone = "p",
                           params = rlang::quos(bonf_alt1 = pval / 2,
                                                bonf_alt2 = pval / 4))
    expect_equal(names(bd_exp$methods), c("bonf", "bonf_alt1", "bonf_alt2"))
    ## check expanded param changed (note: values equal but not identical)
    expect_equal(bd_exp$methods$bonf_alt1$params$p, rlang::quo(pval / 2))
    expect_equal(bd_exp$methods$bonf_alt2$params$p, rlang::quo(pval / 4))
    ## check other param unchanged
    expect_identical(bd_exp$methods$bonf$params$method,
                     bd_exp$methods$bonf_alt2$params$method)
    ## check defined methods are valid and don't break buildBench call
    expect_is(buildBench(bd_exp), "SummarizedBenchmark")
    
    ## check method expansion w/ replacing original method
    bd_replace <- expandMethod(bd, "bonf", onlyone = "p",
                               params = rlang::quos(bonf_alt1 = pval / 2,
                                                    bonf_alt2 = pval / 4),
                               .replace = TRUE)
    expect_equal(names(bd_replace$methods), c("bonf_alt1", "bonf_alt2"))
    ## check defined methods are valid and don't break buildBench call
    expect_is(buildBench(bd_replace), "SummarizedBenchmark")

    ## check method expansion w/ mult params
    bd_mult <- expandMethod(bd, "bonf",
                            params = list(bonf_alt1 = rlang::quos(p = pval / 2),
                                          bonf_alt2 = rlang::quos(p = pval / 4,
                                                                  method = "BH")))
    expect_equal(names(bd_mult$methods), c("bonf", "bonf_alt1", "bonf_alt2"))
    ## check params changed only for expected (note: values equal but not identical)
    expect_equal(bd_mult$methods$bonf_alt1$params$p, rlang::quo(pval / 2))
    expect_equal(bd_mult$methods$bonf_alt1$params$method, rlang::quo("bonferroni"))
    expect_equal(bd_mult$methods$bonf_alt2$params$p, rlang::quo(pval / 4))
    expect_equal(bd_mult$methods$bonf_alt2$params$method, rlang::quo("BH"))
    ## check defined methods are valid and don't break buildBench call
    expect_is(buildBench(bd_mult), "SummarizedBenchmark")

    ## check method expansion w/ mult params and overwriting all 'params' 
    bd_ow <- expandMethod(bd, "bonf",
                          params = list(bonf_alt1 = rlang::quos(p = pval / 2),
                                        bonf_alt2 = rlang::quos(p = pval / 4,
                                                                method = "BH")),
                          .overwrite = TRUE)
    expect_equal(names(bd_ow$methods), c("bonf", "bonf_alt1", "bonf_alt2"))
    ## check params changed only for expected (note: values equal but not identical)
    expect_equal(bd_ow$methods$bonf_alt1$params$p, rlang::quo(pval / 2))
    expect_null(bd_ow$methods$bonf_alt1$params$method)
    expect_equal(bd_ow$methods$bonf_alt2$params$p, rlang::quo(pval / 4))
    expect_equal(bd_ow$methods$bonf_alt2$params$method, rlang::quo("BH"))
    ## check defined methods are valid and don't break buildBench call
    expect_is(buildBench(bd_ow), "SummarizedBenchmark")

    ## check handling of non-parameter "special" values (func, post, meta)
    bd_spec <- expandMethod(bd, "bonf",
                            params = list(
                                bonf_alt1 = rlang::quos(bd.func = function(p, method) { p },
                                                        bd.post = p.adjust,
                                                        bd.meta = list(new_purpose = "test special values")),
                                bonf_alt2 = rlang::quos(bd.func = function(p, method) { p / 2 },
                                                        bd.post = function(x) { x * 2 },
                                                        bd.meta = list(new_purpose = "test special values again"))),
                            .replace = TRUE)
    expect_equal(names(bd_spec$methods), c("bonf_alt1", "bonf_alt2"))
    expect_equal(bd_spec$methods$bonf_alt1$func, rlang::quo(function(p, method) { p }))
    expect_equal(bd_spec$methods$bonf_alt1$post, rlang::quo(p.adjust))
    expect_equal(bd_spec$methods$bonf_alt1$meta, list(new_purpose = "test special values"))
    expect_equal(bd_spec$methods$bonf_alt2$func, rlang::quo(function(p, method) { p / 2 }))
    expect_equal(bd_spec$methods$bonf_alt2$post, rlang::quo(function(x) { x * 2}))
    expect_equal(bd_spec$methods$bonf_alt2$meta, list(new_purpose = "test special values again"))
    ## check defined methods are valid and don't break buildBench call
    expect_is(buildBench(bd_spec), "SummarizedBenchmark")
    
    ## check error when invalid method specified
    expect_error(expandMethod(bd, label = "apple",
                              params = list(bonf_alt1 = rlang::quos(p = pval / 2))),
                              "not defined")

    ## check error when name conflicts introduced
    expect_error(expandMethod(bd, label = "bonf",
                              params = list(bonf = rlang::quos(p = pval / 2))),
                 "should not overlap")
    expect_error(expandMethod(bd, label = "bonf",
                              params = list(rlang::quos(p = pval / 2))),
                 "must be named")
    expect_error(expandMethod(bd, label = "bonf",
                              params = list(bonf_alt = rlang::quos(p = pval / 2),
                                            bonf_alt = rlang::quos(p = pval / 4))),
                 "must be unique")
})

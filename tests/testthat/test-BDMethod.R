library(SummarizedBenchmark)
context("BDMethod and BDMethodList")

## sample SummarizedBenchmark object
data(quantSB)

test_that("BDMethod constructor accepts all valid input", {
    ## simplest call
    expect_silent(bdm <- BDMethod(x = base::identity))
    expect_is(bdm, "BDMethod")
    expect_equal(rlang::expr_text(bdm@fc), "base::identity")
    expect_identical(bdm@f, base::identity)
    expect_true(rlang::is_quosures(bdm@params))
    expect_null(bdm@post)
    expect_null(bdm@meta)
    
    ## call with all params specified
    expect_silent(bdm <- BDMethod(x = base::identity,
                                  params = rlang::quos(x = y),
                                  post = list(p1 = base::identity,
                                              p2 = base::identity),
                                  meta = list(m1 = "a")))
    expect_is(bdm, "BDMethod")
    expect_identical(bdm@f, base::identity)
    expect_true(rlang::is_quosures(bdm@params))
    expect_is(bdm@post, "list")
    expect_is(bdm@meta,"list")

    ## call with quoted function
    expect_silent(bdm <- BDMethod(x = rlang::quo(base::identity)))
    expect_is(bdm, "BDMethod")
    expect_identical(bdm@f, base::identity)
    expect_equal(rlang::expr_text(bdm@fc), "base::identity")

    ## call with SummarizedBenchmark
    expect_silent(bdm <- BDMethod(allSB))
    expect_is(bdm, "BDMethod")

    ## call with BenchDesign
    expect_silent(bdm <- BDMethod(BenchDesign(allSB), i = 2))
    expect_is(bdm, "BDMethod")
    expect_identical(BDMethod(BenchDesign(allSB)), BDMethod(allSB))
})

test_that("BDMethodList constructor accepts all valid input", {
    bdm1 <- BDMethod(x = base::identity)
    bdm2 <- BDMethod(x = base::sqrt)
    bdm3 <- BDMethod(x = base::exp)

    bd1 <- BenchDesign(m1 = bdm1)
    bd2 <- BenchDesign(m2 = bdm2, m3 = bdm3)

    ## simplest call
    expect_silent(bdml1 <- BDMethodList(m1 = bdm1))
    expect_silent(bdml2 <- BDMethodList(m2 = bdm2, m3 = bdm3))
    expect_is(bdml1, "BDMethodList")
    expect_is(bdml2, "BDMethodList")
    expect_identical(bdml1[[1]], bdm1)
    expect_identical(bdml2[[2]], bdm3)

    ## call with BenchDesign
    expect_silent(bd1_bdml <- BDMethodList(bd1))
    expect_identical(bd1_bdml, bdml1)
    expect_silent(bd2_bdml <- BDMethodList(bd2))
    expect_identical(bd2_bdml, bdml2)

    ## call with SummarizedBenchmark)
    expect_silent(sb_bdml <- BDMethodList(allSB))
    expect_is(sb_bdml, "BDMethodList")

    ## combine bdm and bdml
    expect_is(BDMethodList(m1 = bdm1, bdml2), "BDMethodList")
    expect_length(BDMethodList(m1 = bdm1, bdml2), 3)

    ## combine bdm and bd
    expect_is(BDMethodList(m2 = bdm2, bd1), "BDMethodList")
    expect_length(BDMethodList(m2 = bdm2, bd1), 2)

    ## combine bdml and bdml
    expect_is(BDMethodList(bdml1, bdml2), "BDMethodList")
    expect_length(BDMethodList(bdml1, bdml2), 3)
    
    ## combine bdml and bd
    expect_is(BDMethodList(bd1, bdml2), "BDMethodList")
    expect_length(BDMethodList(bd1, bdml2), 3)
    
    ## combine bd and bd
    expect_is(BDMethodList(bd1, bd2), "BDMethodList")
    expect_identical(BDMethodList(bd1, bd2), BDMethodList(bd1_bdml, bd2_bdml))
})

test_that("BDMethod/List setters work", {
    bd <- BenchDesign(allSB)
    bdm1 <- BDMethod(x = base::identity)
    bdml <- BDMethodList(m1 = bdm1)

    ## set bdml bdm
    expect_silent(BDMethod(bdml, "newMethod") <- bdm1)
    expect_identical(BDMethod(bdml, i = "newMethod"), bdm1)
    expect_length(bdml, 2)
    
    ## set benchdesign bdm
    expect_silent(BDMethod(bd, "newMethod") <- bdm1)
    expect_identical(BDMethod(bd, i = "newMethod"), bdm1)
    
    ## set benchdesign bdml
    expect_silent(BDMethodList(bd) <- bdml)
    expect_identical(BDMethodList(bd), bdml)
})

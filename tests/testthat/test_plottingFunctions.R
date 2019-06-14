context( "plottingFunctions" )
test_that( "test that 'plotROC' and 'plotOverlaps'  behave as expected", {
  data( sb )
  sb <- addPerformanceMetric( sb, evalMetric=c("rejections", "TPR", "TNR", "FDR", "FNR"), assay="qvalue" )
  expect_error( plotROC( 1:10 ) )
  sb2 <- sb
  groundTruths( sb2 ) <- NULL
  expect_error( plotROC( sb2 ) )
  sb2 <- sb
  assayNames( sb2 ) <- paste0( "assay", seq_along( assayNames( sb2 ) ) )
  expect_error( plotROC( sb2 ) )
  expect_error( plotMethodsOverlap( sb2 ) )
  expect_error( plotMethodsOverlap( object=1:10 ) )
  expect_error( plotMethodsOverlap( sb, alpha="a" ) )
  expect_error( plotMethodsOverlap( sb, alpha=3 ) )
  expect_identical(
    plotROC( sb )$geom$objname,
    plotROC( sb2, assay="assay1" )$geom$objname )
  expect_is( plotMethodsOverlap( sb ), "upset" )
  expect_is( plotMethodsOverlap( sb2, assay="assay1" ), "upset" )
} )

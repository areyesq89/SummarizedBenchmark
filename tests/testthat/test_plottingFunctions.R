context( "plottingFunctions" )
test_that( "test that 'plotROC' and 'plotOverlaps'  behave as expected", {
  data( sb )
  sb <- addDefaultMetrics( sb )
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
} )

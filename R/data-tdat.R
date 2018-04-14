#' Example data.frame containing results for 50 two-sample t-tests.
#'
#' @format
#' a data.frame that contains the results of 50 simulated two-sample
#' t-tests, with each row corresponding to an independent test. The
#' data.frame includes the following 5 columns:
#' 1. H = binary 0/1 whether data for the test was simulated under the null (0) or alternative (1)
#' 2. test_statistic = test-statistics of the t-test
#' 3. effect_size = mean difference between the two sample groups
#' 4. pval = p-value of the t-test
#' 5. SE = standard error of the t-test
#' 
#' @name tdat
#' @docType data
#' @keywords data
#' @examples
#' data(tdat)
NULL

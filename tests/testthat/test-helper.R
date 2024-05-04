cat("Testing SelectedbdData() function \n")
# testthat::test_that("errors", {
#   cat("Verify message of NULL values catched correctly or not \n")
#   testthat::expect_message(
#     SelectedbdData(NULL, NULL),
#     "invalid parameter specified"
#   )
#   cat("Verify when correct parameters are given appropriate data is genrated or not \n")
#   testthat::expect_gt(nrow(SelectedbdData("Common Reed Bunting", "Emberiza schoeniclus (Linnaeus, 1758)")), 0)
#   testthat::expect_gt(ncol(SelectedbdData("Common Reed Bunting", "Emberiza schoeniclus (Linnaeus, 1758)")), 0)
# })

cat("Testing monthlyOccurence() function \n")
# testthat::test_that("errors", {
#   cat("Verify error of invalid values catched correctly or not \n")
#   testthat::expect_message(
#     monthlyOccurence(NULL),
#     "please enter non-null data"
#   )
#   cat("Verify when correct parameters are given appropriate data is genrated or not \n")
#   testthat::expect_lte(nrow(monthlyOccurence(bdData)), 12)
#   testthat::expect_equal(ncol(monthlyOccurence(bdData)), 2)
# })

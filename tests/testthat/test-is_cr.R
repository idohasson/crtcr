# library(testthat)
#
# test_that("is_cr() returns a logical vector indicating if a value appears more than once in a vector", {
#   expect_true(all(is_cr(c(1, 2, 3, 1)) == c(TRUE, FALSE, FALSE, TRUE)))
#   expect_true(all(is_cr(c(1, 2, 3, 2, 3)) == c(FALSE, TRUE, TRUE, TRUE, TRUE)))
#   expect_true(all(is_cr(c(1, 1, 1, 1)) == c(TRUE, TRUE, TRUE, TRUE)))
#   expect_true(all(is_cr(c(1, 2, 3, 4)) == c(FALSE, FALSE, FALSE, FALSE)))
#
#   # test with additional vectors
#   expect_true(all(is_cr(c(1, 2, 3, 1), c(1, 2, 3, 1)) == c(TRUE, FALSE, FALSE, TRUE)))
#   expect_true(all(is_cr(c(1, 2, 3, 2, 3), c(1, 2, 3, 2, 3)) == c(FALSE, TRUE, TRUE, TRUE, TRUE)))
#   expect_true(all(is_cr(c(1, 1, 1, 1), c(1, 1, 1, 1)) == c(TRUE, TRUE, TRUE, TRUE)))
#   expect_true(all(is_cr(c(1, 2, 3, 4), c(1, 2, 3, 4)) == c(FALSE, FALSE, FALSE, FALSE)))
# })

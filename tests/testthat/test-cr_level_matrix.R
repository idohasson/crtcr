test_that("multiplication works", {

  l <- replicate(6, sample(LETTERS, sample(13:18, 1, replace = TRUE)), simplify = FALSE)
  mat <- cr_level_table(l) # number of vectors a letter found in

  expect_true(is.matrix(mat))
  # expect_equal(2 * 2, 4)
})

test_that("multiplication works", {

  l <- replicate(10, sample(LETTERS, sample(8:15, 1)), simplify = FALSE) # lists of random letters
  df <- share_level(l)

  expect_true(is.data.frame(df))

  expect_equal(nrow(df), length(unique(unlist(l))))
})

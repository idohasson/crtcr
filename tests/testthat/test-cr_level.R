library(stringi)

rand_aa <- function(vec_size, l=5) {
  do.call(paste0, Map(stri_rand_strings, n=vec_size, length=l, pattern = '[A-C]'))
}

test_that("works on a character vector", {
  v <- rand_aa(10)

  cl <- cr_level(v)

  expect_true(is.data.frame(cl))

  expect_equal(nrow(cl), length(unique(v)))

})


test_that("multiplication works", {


  expect_equal(2 * 2, 4)
})

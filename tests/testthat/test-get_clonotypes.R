test_that("right argument input", {


  expect_error(get_clonotypes(list()))
  expect_error(get_clonotypes(NULL))
  expect_error(get_clonotypes(as.matrix()))

})

test_that("column is given but not in the data frame", {
  # column is given but not in the data frame
  expect_no_error(get_clonotypes(data.frame(in_df=TRUE), "in_df"))
  expect_error(get_clonotypes(data.frame(in_df=TRUE), "not_in_df"))
})

# test_that("column is not given and couldn't find any in the package's 'known collection'", {
#
#   expect_no_error(get_clonotypes(data.frame(cdr3aa=TRUE)))
#   expect_error(get_clonotypes(data.frame(unknown_col=TRUE)))
#
# })


test_that("vctor from dataframe", {

  df <- rand_rep_df(seq_n = 100, seq_len=3)
  # "Column is not provided and coulnt detect known column name"
  expect_error(get_clonotypes(df))
  # expect_vector(get_clonotypes(df, "aa"))

  # expect_true(is.data.frame(get_clonotypes(df, clonotype_col="aa", clone_col="nt")))

  # expect_equal(2 * 2, 4)
})

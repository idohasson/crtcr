
test_that("cr level", {

  AA_list <- c("A","C","D","E","F","G","H","I","K","L","M","N","P","Q","R","S","T","V","W","Y")
  r_aa <- function(size) paste(sample(AA_list, size, replace = TRUE), collapse = "")

  n <- 1000
  df <- data.frame(aa = replicate(n, r_aa(2)),
             var1 = sample(5, replace = TRUE),
             var2 = sample(5, n, replace = TRUE),
             var3 = sample(5, n, replace = TRUE))

  cr <- cr_level(df, "aa", c("var1", "var2"))

  expect_true(is.data.frame(cr))

})


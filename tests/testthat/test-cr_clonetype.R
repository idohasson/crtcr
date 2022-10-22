test_that("multiplication works", {

  AA_list <- c("A","C","D","E","F","G","H","I","K","L","M","N","P","Q","R","S","T","V","W","Y")
  r_aa <- function(size) paste(sample(AA_list, size, replace = TRUE), collapse = "")

  c1 <- replicate(1000, r_aa(3))
  c2 <- replicate(1000, r_aa(3))

  cr <- cr_clonotype(c1, c2)

  expect_true(is.list(cr))

})

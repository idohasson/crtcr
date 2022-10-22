test_that("multiplication works", {

  v1 <- c("A", "B")
  v2 <- c("B", "C", "C")

  result <- list(private = c("A"),
                 inclusive = c("B"),
                 exclusive = c("C"))

  expect_equal(cr_public(v1, v2), result)
})

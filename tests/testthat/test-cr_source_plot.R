test_that("multiplication works", {
  nt <- c("A", "G", "C", "T")
  seq_len <- 6
  seq_n <- c(12, 7)

  clone1 <- replicate(seq_n[1], paste(sample(c("A", "G", "C", "T"), seq_len, replace = TRUE), collapse = ''))
  clone2 <- replicate(seq_n[2], paste(sample(c("A", "G", "C", "T"), seq_len, replace = TRUE), collapse = ''))

  il <- list(g1 = clone1, g2 = clone2)

  cr_source(il)


})

# Load the testthat library
library(testthat)

# Define a test for the calculate_clonotype_diversity() function
test_that("calculate_clonotype_diversity() works correctly", {
  # Create a test TCR repertoire
  repertoire <- data.frame(clonotype = c("A", "A", "B", "B", "C", "C"),
                           sequence = c("AAA", "AAA", "BBB", "BBB", "CCC", "CCC"))

  # Test calculating the diversity of clonotype A
  expect_equal(calculate_clonotype_diversity(repertoire, "A", "sequence", "clonotype"), 1)

  # Test calculating the diversity of clonotype B
  expect_equal(calculate_clonotype_diversity(repertoire, "B", "sequence", "clonotype"), 1)

  # Test calculating the diversity of clonotype C
  expect_equal(calculate_clonotype_diversity(repertoire, "C", "sequence", "clonotype"), 1)

  # Test calculating the diversity of a clonotype not present in the repertoire
  expect_equal(calculate_clonotype_diversity(repertoire, "D", "sequence", "clonotype"), 0)
})

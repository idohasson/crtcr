# Load the testthat library
library(testthat)

# Define a test for the calculate_shared_clonotypes() function
test_that("calculate_shared_clonotypes() works correctly", {
  # Create a test list of TCR repertoires
  repertoire_list <- list(
    data.frame(clonotype = c("A", "A", "B", "B", "C", "C"),
               sequence = c("AAA", "AAA", "BBB", "BBB", "CCC", "CCC")),
    data.frame(clonotype = c("A", "A", "B", "B", "D", "D"),
               sequence = c("AAA", "AAA", "BBB", "BBB", "DDD", "DDD")),
    data.frame(clonotype = c("A", "A", "B", "B", "E", "E"),
               sequence = c("AAA", "AAA", "BBB", "BBB", "EEE", "EEE"))
  )

  # Test calculating the level of sharing of clonotype A
  expect_equal(calculate_shared_clonotypes(repertoire_list, "A"), 1)

  # Test calculating the level of sharing of clonotype B
  expect_equal(calculate_shared_clonotypes(repertoire_list, "B"), 1)

  # Test calculating the level of sharing of clonotype C
  expect_equal(calculate_shared_clonotypes(repertoire_list, "C"), 1/3)

  # Test calculating the level of sharing of clonotype D
  expect_equal(calculate_shared_clonotypes(repertoire_list, "D"), 1/3)

  # Test calculating the level of sharing of clonotype E
  expect_equal(calculate_shared_clonotypes(repertoire_list, "E"), 1/3)

  # Test calculating the level of sharing of a clonotype not present in any repertoire
  expect_equal(calculate_shared_clonotypes(repertoire_list, "F"), 0)

  # This test creates a list of three test TCR repertoires, each with three clonotypes that overlap with one another to varying degrees. It then checks that the `calculate_shared_clonotypes()` function returns the correct level of sharing for each clonotype. It also checks that the function returns 0 for a clonotype that is not present in any of the repertoires.
})


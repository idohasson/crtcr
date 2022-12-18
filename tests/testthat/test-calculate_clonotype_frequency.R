# Define a test for the calculate_clonotype_frequency() function
test_that("calculate_clonotype_frequency() works correctly", {
  # Create a test TCR repertoire
  repertoire <- data.frame(clonotype = c("A", "A", "B", "B", "C", "C"),
                           sequence = c("AAA", "AAA", "BBB", "BBB", "CCC", "CCC"))

  # Test calculating the frequency of clonotype A
  expect_equal(calculate_clonotype_frequency(repertoire, "A", "sequence", "clonotype"), 1/6)

  # Test calculating the frequency of clonotype B
  expect_equal(calculate_clonotype_frequency(repertoire, "B", "sequence", "clonotype"), 1/6)

  # Test calculating the frequency of clonotype C
  expect_equal(calculate_clonotype_frequency(repertoire, "C", "sequence", "clonotype"), 1/6)

  # Test calculating the frequency of a clonotype not present in the repertoire
  expect_equal(calculate_clonotype_frequency(repertoire, "D", "sequence", "clonotype"), 0)
})

test_that("unique_value_count calculates the number of unique values correctly", {

  # Define a vector of values
  value_vector <- c("A", "B", "C", "B", "D", "A", "C")

  # Check that the number of unique values is correct
  expect_equal(unique_value_count(value_vector), 4)

})

test_that("group_value_counts calculates the number of unique values in each group correctly", {

  # Define a vector of values
  value_vector <- c("A", "B", "C", "B", "D", "A", "C")

  # Define a grouping vector
  grouping_vector <- c("Group 1", "Group 2", "Group 1", "Group 2", "Group 1", "Group 2", "Group 1")

  # Check that the number of unique values in each group is correct
  expect_equal(group_value_counts(value_vector, grouping_vector), list("Group 1" = 3, "Group 2" = 2))

})

group_shared_level <- function(data_list, level_func, proportions=FALSE, ignore_na = FALSE, names=FALSE) {

  # Calculate the number of unique values for each vector in the list
  unique_counts <- lapply(data_list, unique_value_count, proportions=proportions, remove_na=ignore_na)

  # Calculate the level of shared values using the provided function
  shared_level <- level_func(unique_counts)

  # If the 'names' argument is set to TRUE, add names to the results
  if (isTRUE(names)) {
    names(shared_level) <- names(data_list)
  }

  # Return the calculated level of shared values
  return(shared_level)

}

unique_value_count <- function(value_vector, proportion=FALSE, remove_na = FALSE) {

  # Check if there are duplicated values in the input vector
  has_duplicates <- duplicated(value_vector)

  # If the 'remove_na' argument is set to TRUE, find the indices of non-NA values in the vector
  if (isTRUE(remove_na)) {
    non_na_indices <- vctrs::vec_detect_complete(value_vector)
    has_duplicates <- has_duplicates & non_na_indices
  }

  # Count the number of unique nucleotide sequences
  unique_count <- sum(!has_duplicates)

  # If the 'proportion' argument is set to TRUE, return the proportion of unique nucleotide sequences,
  # otherwise return the count
  if (isTRUE(proportion)) {
    return(unique_count / vec_size(value_vector))
  } else {
    return(unique_count)
  }

}

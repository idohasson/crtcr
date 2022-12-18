#' Calculates the number of unique values in a given vector
#'
#' This function calculates the number of unique values in a given vector.
#' The calculation can be adjusted to exclude NA values and return the proportion of unique values instead of the count.
#'
#' @param value_vector A vector of values
#' @param proportion A logical indicating whether to return the proportion of unique values
#' @param remove_na A logical indicating whether to exclude NA values from the calculation
#' @return The number of unique values in the input vector, or the proportion of unique values
#'         if the 'proportion' argument is set to TRUE
#' @export
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


#' Calculates the number of unique values in each group of a given vector
#'
#' This function splits a vector of values into groups based on a grouping vector, and then calculates the number
#' of unique values in each group using the 'unique_value_count' function. The type of values
#' (nucleotide or amino acid) is automatically detected based on the characters in the vector.
#' The calculation can be adjusted to exclude NA values and return the proportion of unique values instead of the count.
#'
#' @param value_vector A vector of nucleotide or amino acid values
#' @param grouping_vector A vector indicating which group each element of the 'value_vector' belongs to
#' @param proportion A logical indicating whether to return the proportion of unique values
#' @param remove_na A logical indicating whether to exclude NA values from the calculation
#' @return A vector of the number of unique values in each group of the input vector, or the proportion of unique values
#'         in each group if the 'proportion' argument is set to TRUE
#' @export
group_value_counts <- function(value_vector, grouping_vector, proportion=FALSE, remove_na = FALSE) {
  # Check if the 'value_vector' and 'grouping_vector' have the same length
  if (length(value_vector) != length(grouping_vector)) {
    stop("The 'value_vector' and 'grouping_vector' must have the same length.")
  }

  # Split the 'value_vector' into groups based on the 'grouping_vector'
  split_sequences <- split(value_vector, grouping_vector)

  # Calculate the number of unique value in each group using the 'unique_value_count ' function
  unique_counts <- lapply(split_sequences, unique_sequence_count, proportion=proportion, remove_na=remove_na)

  # Return the calculated counts
  return(unique_counts)

}

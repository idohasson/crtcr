#' Calculates the number of unique nucleotide sequences in a given nucleotide vector
#'
#' @param nucleotide_vector A vector of nucleotide sequences
#' @param proportion A logical indicating whether to return the proportion of unique sequences
#' @param remove_na A logical indicating whether to exclude NA values from the calculation
#' @return The number of unique nucleotide sequences in the input vector, or the proportion of unique sequences
#'         if the 'proportion' argument is set to TRUE
#' @export

converged_nucleotide_count <- function(nucleotide_vector, proportion=FALSE, remove_na = FALSE) {

  # Check if there are duplicate nucleotide sequences in the input vector
  has_duplicates <- duplicated(nucleotide_vector)

  # If the 'remove_na' argument is set to TRUE, find the indices of non-NA values in the vector
  if (isTRUE(remove_na)) {
    non_na_indices <- vctrs::vec_detect_complete(nucleotide_vector)
    has_duplicates <- has_duplicates & non_na_indices
  }

  # Count the number of unique nucleotide sequences
  unique_count <- sum(!has_duplicates)

  # If the 'proportion' argument is set to TRUE, return the proportion of unique nucleotide sequences,
  # otherwise return the count
  if (isTRUE(proportion)) {
    return(unique_count / vec_size(nucleotide_vector))
  } else {
    return(unique_count)
  }

}



#' Calculates the number of unique amino acid sequences in a given vector
#'
#' This function calculates the number of unique amino acid sequences in a given vector of amino acid sequences.
#' The calculation can be adjusted to exclude NA values and return the proportion of unique sequences instead of the count.
#'
#' @param aa_vector A vector of amino acid sequences
#' @param proportion A logical indicating whether to return the proportion of unique sequences
#' @param remove_na A logical indicating whether to exclude NA values from the calculation
#' @return The number of unique amino acid sequences in the input vector, or the proportion of unique sequences
#'         if the 'proportion' argument is set to TRUE
#' @export

unique_aa_count <- function(aa_vector, proportion=FALSE, remove_na = FALSE) {

  # Check if there are duplicate amino acid sequences in the input vector
  has_duplicates <- duplicated(aa_vector)

  # If the 'remove_na' argument is set to TRUE, find the indices of non-NA values in the vector
  if (isTRUE(remove_na)) {
    non_na_indices <- vctrs::vec_detect_complete(aa_vector)
    has_duplicates <- has_duplicates & non_na_indices
  }

  # Count the number of unique amino acid sequences
  unique_count <- sum(!has_duplicates)

  # If the 'proportion' argument is set to TRUE, return the proportion of unique amino acid sequences,
  # otherwise return the count
  if (isTRUE(proportion)) {
    return(unique_count / vec_size(aa_vector))
  } else {
    return(unique_count)
  }

}


#' Calculates the number of unique nucleotide sequences coding for a given amino acid sequence
#'
#' This function calculates the number of unique nucleotide sequences that code for a given amino acid sequence.
#' The calculation can be adjusted to exclude NA values and return the proportion of unique sequences instead of the count.
#'
#' @param nucleotide_vector A vector of nucleotide sequences coding for the same amino acid sequence
#' @param proportion A logical indicating whether to return the proportion of unique sequences
#' @param remove_na A logical indicating whether to exclude NA values from the calculation
#' @return The number of unique nucleotide sequences in the input vector, or the proportion of unique sequences
#'         if the 'proportion' argument is set to TRUE
#' @export

unique_nucleotide_count <- function(nucleotide_vector, proportion=FALSE, remove_na = FALSE) {

  # Check if there are duplicate nucleotide sequences in the input vector
  has_duplicates <- duplicated(nucleotide_vector)

  # If the 'remove_na' argument is set to TRUE, find the indices of non-NA values in the vector
  if (isTRUE(remove_na)) {
    non_na_indices <- vctrs::vec_detect_complete(nucleotide_vector)
    has_duplicates <- has_duplicates & non_na_indices
  }

  # Count the number of unique nucleotide sequences
  unique_count <- sum(!has_duplicates)

  # If the 'proportion' argument is set to TRUE, return the proportion of unique nucleotide sequences,
  # otherwise return the count
  if (isTRUE(proportion)) {
    return(unique_count / vec_size(nucleotide_vector))
  } else {
    return(unique_count)
  }

}



#' Calculates the number of unique sequences in a given vector
#'
#' This function calculates the number of unique sequences in a given vector of sequences. The type of sequences
#' (nucleotide or amino acid) is automatically detected based on the characters in the vector.
#' The calculation can be adjusted to exclude NA values and return the proportion of unique sequences instead of the count.
#'
#' @param sequence_vector A vector of nucleotide or amino acid sequences
#' @param proportion A logical indicating whether to return the proportion of unique sequences
#' @param remove_na A logical indicating whether to exclude NA values from the calculation
#' @return The number of unique sequences in the input vector, or the proportion of unique sequences
#'         if the 'proportion' argument is set to TRUE
#' @export

unique_sequence_count <- function(sequence_vector, proportion=FALSE, remove_na = FALSE) {

  # Check if the input vector contains nucleotide or amino acid sequences
  is_nucleotide <- all(sequence_vector %in% c("A", "C", "G", "T"))
  is_amino_acid <- all(sequence_vector %in% c("A", "C", "D", "E", "F", "G", "H", "I", "K", "L", "M", "N", "P", "Q", "R", "S", "T", "V", "W", "Y"))

  # If the vector contains neither nucleotide nor amino acid sequences, return an error
  if (!is_nucleotide && !is_amino_acid) {
    stop("The input vector must contain nucleotide or amino acid sequences.")
  }

  # Check if there are duplicate sequences in the input vector
  has_duplicates <- duplicated(sequence_vector)

  # If the 'remove_na' argument is set to TRUE, find the indices of non-NA values in the vector
  if (isTRUE(remove_na)) {
    non_na_indices <- vctrs::vec_detect_complete(sequence_vector)
    has_duplicates <- has_duplicates & non_na_indices
  }

  # Count the number of unique sequences
  unique_count <- sum(!has_duplicates)

  # If the 'proportion' argument is set to TRUE, return the proportion of unique sequences,
  # otherwise return the count
  if (isTRUE(proportion)) {
    return(unique_count / vec_size(sequence_vector))
  } else {
    return(unique_count)
  }

}


#' Calculates the number of unique sequences in each group of a given vector
#'
#' This function splits a vector of sequences into groups based on a grouping vector, and then calculates the number
#' of unique sequences in each group using the 'unique_sequence_count' function. The type of sequences
#' (nucleotide or amino acid) is automatically detected based on the characters in the vector.
#' The calculation can be adjusted to exclude NA values and return the proportion of unique sequences instead of the count.
#'
#' @param sequence_vector A vector of nucleotide or amino acid sequences
#' @param grouping_vector A vector indicating which group each element of the 'sequence_vector' belongs to
#' @param proportion A logical indicating whether to return the proportion of unique sequences
#' @param remove_na A logical indicating whether to exclude NA values from the calculation
#' @return A vector of the number of unique sequences in each group of the input vector, or the proportion of unique sequences
#'         in each group if the 'proportion' argument is set to TRUE
#' @export

unique_sequence_count_by_group <- function(sequence_vector, grouping_vector, proportion=FALSE, remove_na = FALSE) {

  # Check if the 'sequence_vector' and 'grouping_vector' have the same length
  if (length(sequence_vector) != length(grouping_vector)) {
    stop("The 'sequence_vector' and 'grouping_vector' must have the same length.")
  }

  # Split the 'sequence_vector' into groups based on the 'grouping_vector'
  split_sequences <- split(sequence_vector, grouping_vector)

  # Calculate the number of unique sequences in each group using the 'unique_sequence_count' function
  unique_counts <- lapply(split_sequences, unique_sequence_count, proportion=proportion, remove_na=remove_na)

  # Return the calculated counts
  return(unique_counts)

}

#' Calculates the number of unique sequences in each group of a given data frame column
#'
#' This function splits a column of a data frame into groups based on the values in another column,
#' and then calculates the number of unique sequences in each group using the 'unique_sequence_count' function.
#' The type of sequences (nucleotide or amino acid) is automatically detected based on the characters in the vector.
#' The calculation can be adjusted to exclude NA values and return the proportion of unique sequences instead of the count.
#'
#' @param data A data frame containing the columns with the sequences and the grouping values
#' @param sequence_column The name of the column containing the sequences
#' @param grouping_column The name of the column containing the grouping values
#' @param proportion A logical indicating whether to return the proportion of unique sequences
#' @param remove_na A logical indicating whether to exclude NA values from the calculation
#' @return A list of vectors containing the number of unique sequences in each group of the input data frame column,
#'         or the proportion of unique sequences in each group if the 'proportion' argument is set to TRUE
#' @export


unique_sequence_count_by_group_df <- function(data, sequence_column, grouping_column, proportion=FALSE, remove_na = FALSE) {

  # Check if the specified columns exist in the data frame
  if (!sequence_column %in% colnames(data)) {
    stop(paste0("The column '", sequence_column, "' does not exist in the input data frame."))
  }
  if (!grouping_column %in% colnames(data)) {
    stop(paste0("The column '", grouping_column, "' does not exist in the input data frame."))
  }

  # Extract the 'sequence_column' and 'grouping_column' as vectors
  sequence_vector <- data[[sequence_column]]
  grouping_vector <- data[[grouping_column]]

  # Split the 'sequence_vector' into groups based on the 'grouping_vector'
  split_sequences <- split(sequence_vector, grouping_vector)

  # Calculate the number of unique sequences in each group using the 'unique_sequence_count' function
  unique_counts <- lapply(split_sequences, unique_sequence_count, proportion=proportion, remove_na=remove_na)

  # Return the calculated counts
  return(unique_counts)

}

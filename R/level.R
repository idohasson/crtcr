#' @title Compute Numeric Vector Levels
#'
#' @description This function computes the levels of a numeric vector using a provided function for generating the vector. The levels are then computed using the `compute_vector_levels` function, which uses a provided function or formula for computing vector intervals. An optional threshold function can be applied to the interval function output to specify a threshold for the computed levels.
#'
#' @param x a vector of data
#' @param vec_func a function for generating a numeric vector from the data
#' @param interval_func a function or formula for computing vector intervals
#' @param threshold_func an optional function for applying a threshold to the computed levels
#' @param ... additional arguments passed to the `interval_func` and `threshold_func` functions
#' @param remove_duplicates a logical indicating whether to remove duplicate levels
#'
#' @return a numeric vector of levels
#'
#' @examples
#' x <- c("AA", "AB", "BB", "AA", "BB", "AB")
#'
#' # Count the number of each unique nucleotide sequence group
#' count_seq_groups <- function(seq) table(seq)
#'
#' # Compute levels using quantile intervals
#' compute_numeric_vector_levels(x, vec_func = count_seq_groups, interval_func = quantile)
#'
#' # Compute levels using custom intervals
#' custom_intervals <- function(vec) c(min(vec), mean(vec), max(vec))
#' compute_numeric_vector_levels(x, vec_func = count_seq_groups, interval_func = custom_intervals)
#'
#' # Compute levels using custom intervals and a threshold function
#' custom_intervals <- function(vec) c(min(vec), mean(vec), max(vec))
#' threshold_func <- function(levels) levels[levels > 1]
#' compute_numeric_vector_levels(x, vec_func = count_seq_groups, interval_func = custom_intervals, threshold_func = threshold_func)
#'
compute_numeric_vector_levels <- function(x, vec_func, interval_func, threshold_func=NULL, ..., remove_duplicates=TRUE) {
  # Generate a numeric vector from the data
  vec <- vec_func(x)

  # Compute vector levels
  levels <- compute_vector_levels(vec, interval_func, ..., remove_duplicates=remove_duplicates)

  # Apply threshold function, if provided
  if (!is.null(threshold_func)) {
    levels <- threshold_func(levels)
  }

  levels
}


#' @title Compute Vector Levels
#'
#' @description This function computes the levels of a numeric vector using a provided function or formula for computing vector intervals. The levels are computed by sorting the vector intervals and removing any duplicate levels if specified.
#'
#' @param x a numeric vector
#' @param interval_func a function or formula for computing vector intervals
#' @param ... additional arguments passed to the `interval_func` function
#' @param remove_duplicates a logical indicating whether to remove duplicate levels
#'
#' @return a numeric vector of levels
#'
#' @examples
#' x <- c(1, 2, 3, 4, 5, 6)
#'
#' # Compute levels using quantile intervals
#' compute_vector_levels(x, interval_func = quantile)
#'
#' # Compute levels using custom intervals
#' custom_intervals <- function(vec) c(min(vec), mean(vec), max(vec))
#' compute_vector_levels(x, interval_func = custom_intervals)
#'
#' @export
#'
compute_vector_levels <- function(x, interval_func=quantile, ..., remove_duplicates=TRUE) {
  # Convert interval_func to a function if it is a formula
  if (is_formula(interval_func)) {
    interval_func <- as_function(interval_func)
  }

  # Compute vector intervals using the provided function
  if (is_function(interval_func)) {
    interval_func <- interval_func(x, ...)
  }

  # Sort the vector intervals
  levels <- sort(interval_func)

  # Remove duplicate levels if specified
  if (isTRUE(remove_duplicates)) {
    levels <- levels[!duplicated(levels)]
  }

  # Compute the level of the numeric vector
  findInterval(x, levels, rightmost.closed = TRUE)
}


#' @title Compute Numeric Vector Levels
#'
#' @description This function computes the levels of a numeric vector using a provided function for generating the vector. The levels are then computed using the `compute_vector_levels` function, which uses a provided function or formula for computing vector intervals.
#'
#' @param x a vector of data
#' @param vec_func a function for generating a numeric vector from the data
#' @param interval_func a function or formula for computing vector intervals
#' @param ... additional arguments passed to the `interval_func` function
#' @param remove_duplicates a logical indicating whether to remove duplicate levels
#'
#' @return a numeric vector of levels
#'
#' @examples
#' x <- c("AA", "AB", "BB", "AA", "BB", "AB")
#'
#' # Count the number of each unique nucleotide sequence group
#' count_seq_groups <- function(seq) table(seq)
#'
#' # Compute levels using quantile intervals
#' compute_numeric_vector_levels(x, vec_func = count_seq_groups, interval_func = quantile)
#'
#' # Compute levels using custom intervals
#' custom_intervals <- function(vec) c(min(vec), mean(vec), max(vec))
#' compute_numeric_vector_levels(x, vec_func = count_seq_groups, interval_func = custom_intervals)
#'
#' @export
#'
compute_numeric_vector_levels <- function(x, vec_func, interval_func=quantile, ..., remove_duplicates=TRUE) {
  # Generate a numeric vector using the provided function
  vec <- vec_func(x)

  # Compute levels of the numeric vector using the compute_vector_levels function
  compute_vector_levels(vec, interval_func = interval_func, ..., remove_duplicates = remove_duplicates)
}

# Count the number of each unique nucleotide sequence group
convergent_recombination_level <- function(seq) table(seq)

# Compute levels using quantile intervals
compute_numeric_vector_levels(x, vec_func = convergent_recombination_level, interval_func = quantile)

# Compute levels using custom intervals
custom_intervals <- function(vec) c(min(vec), mean(vec), max(vec))
compute_numeric_vector_levels(x, vec_func = convergent_recombination_level, interval_func = custom_intervals)

#' Calculate the number of unique sequences coding for a given clonotype in a TCR repertoire
#'
#' @param repertoire a data frame containing the TCR repertoire, with columns for the clonotype and the sequence
#' @param clonotype a character string specifying the clonotype to calculate the diversity for
#' @return an integer representing the number of unique sequences coding for the specified clonotype
#' @export
calculate_clonotype_diversity <- function(repertoire, clonotype) {
  # Select rows in the repertoire data frame that correspond to the specified clonotype
  clonotype_sequences <- subset(repertoire, clonotype == clonotype)

  # Count the number of unique sequences in the subset
  unique_count <- n_distinct(clonotype_sequences$sequence)

  # Return the unique count as output
  return(unique_count)
}

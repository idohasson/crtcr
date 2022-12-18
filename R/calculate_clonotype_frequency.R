#' Calculate the fraction of the total number of sequences in a TCR repertoire that are represented by a given clonotype
#'
#' @param repertoire a data frame containing the TCR repertoire, with columns for the clonotype and the sequence
#' @param clonotype a character string specifying the clonotype to calculate the frequency for
#' @return a numeric value representing the fraction of the total number of sequences in the repertoire that are represented by the specified clonotype
#' @export
calculate_clonotype_frequency <- function(repertoire, clonotype, clone_col, clonotype_col) {
  # Calculate the number of unique sequences coding for the clonotype using the calculate_clonotype_diversity function
  clonotype_diversity <- calculate_clonotype_diversity(repertoire, clonotype, clone_col, clonotype_col)

  # Count the total number of sequences in the repertoire
  total_count <- nrow(repertoire)

  # Calculate the frequency of the clonotype as the number of unique sequences divided by the total number of sequences
  frequency <- clonotype_diversity / total_count

  # Return the frequency as output
  return(frequency)
}

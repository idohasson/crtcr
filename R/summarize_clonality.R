#' Summarize the clonality of a TCR repertoire
#'
#' @param repertoire a data frame containing the TCR repertoire, with columns for the clonotype and the sequence
#' @param clonotype
#' @param sequence
#'
#' @return a data frame containing clonality measures (e.g., number of unique sequences, frequency, and level of sharing) for each clonotype in the repertoire
#' @export
summarize_clonality <- function(repertoire, clonotype, sequence) {
  # Group the repertoire by clonotype
  repertoire_by_clonotype <- dplyr::group_by(repertoire, enquote(clonotype))

  # Calculate clonality measures for each group
  clonality_summary <- dplyr::summarize(repertoire_by_clonotype,
                                 unique_seqs = dplyr::n_distinct(sequence),
                                 frequency = dplyr::n_distinct(sequence) / dplyr::n(),
                                 level_of_sharing = dplyr::n())

  # Return the clonality summary as output
  return(clonality_summary)
}

# repertoire <- data.frame(clonotype = c("A", "A", "B", "B", "C", "C"),
#                          sequence = c("AAA", "AAA", "BBB", "BBB", "CCC", "CCC"))
# repertoire_list <- list(
#   data.frame(clonotype = c("A", "A", "B", "B", "C", "C"),
#              sequence = c("AAA", "AAA", "BBB", "BBB", "CCC", "CCC")),
#   data.frame(clonotype = c("A", "A", "B", "B", "D", "D"),
#              sequence = c("AAA", "AAA", "BBB", "BBB", "DDD", "DDD")),
#   data.frame(clonotype = c("A", "A", "B", "B", "E", "E"),
#              sequence = c("AAA", "AAA", "BBB", "BBB", "EEE", "EEE"))
# )
# calculate_shared_clonotypes(list(repertoire, repertoire, repertoire), "A")

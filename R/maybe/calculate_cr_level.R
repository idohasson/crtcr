calculate_average_cr_level <- function(cr_levels) {
  # Calculate the average CR level
  average_cr_level <- mean(cr_levels)
  
  return(average_cr_level)
}

calculate_cr_level <- function(aa_sequence, nt_sequences) {
  # Remove duplicates from the NT sequences
  nt_sequences <- unique(nt_sequences)
  
  # Calculate the CR level for the given AA sequence
  cr_level <- length(nt_sequences[aa_sequence %in% nt_sequences])
  
  return(cr_level)
}

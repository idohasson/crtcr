standardize_format <- function(sequences, format) {
  # Ensure that all sequences are in the same format
  if (format == "nucleotide") {
    sequences <- gsub("[^ATGC]", "-", sequences, perl = TRUE)
  } else if (format == "amino_acid") {
    sequences <- gsub("[^ARNDCQEGHILKMFPSTWYV]", "-", sequences, perl = TRUE)
  }
  return(sequences)
}

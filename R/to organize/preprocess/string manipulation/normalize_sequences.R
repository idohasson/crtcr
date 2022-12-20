normalize_sequences <- function(sequences, method, reference) {
  # Normalize sequences to specified length or reference sequence
  if (method == "trim") {
    # Trim sequences to specified length
    sequences <- trim_sequences(sequences, length)
  } else if (method == "pad") {
    # Pad sequences to specified length
    sequences <- paste0(sequences, rep("-", length - nchar(sequences)))
  } else if (method == "align") {
    # Align sequences to reference sequence
    sequences <- vmatchPattern(sequences, reference)
  }
  return(sequences)
}

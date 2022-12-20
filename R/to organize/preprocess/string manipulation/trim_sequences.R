trim_sequences <- function(sequences, length) {
  # Trim sequences to specified length
  sequences <- substr(sequences, 1, length)
  return(sequences)
}

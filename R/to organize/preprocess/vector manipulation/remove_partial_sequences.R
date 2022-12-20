remove_partial_sequences <- function(sequences, min_length) {
  # Remove sequences that are shorter than specified length or have gaps within the sequence
  sequences <- sequences[nchar(sequences) >= min_length & !grepl("-", sequences)]
  return(sequences)
}

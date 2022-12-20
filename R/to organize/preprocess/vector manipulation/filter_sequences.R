filter_sequences <- function(sequences, min_length, max_length, min_count, max_count, ...) {
  # Filter sequences based on user-specified criteria
  sequences <- sequences[nchar(sequences) >= min_length & nchar(sequences) <= max_length]
  sequences <- sequences[table(sequences) >= min_count & table(sequences) <= max_count]
  return(sequences)
}

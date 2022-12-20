clean_sequences <- function(sequences) {
  # Remove any non-alphabetic characters and convert to uppercase
  sequences <- gsub("[^[:alpha:]]", "", sequences, perl = TRUE)
  sequences <- toupper(sequences)
  return(sequences)
}

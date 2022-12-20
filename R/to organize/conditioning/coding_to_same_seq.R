#' Check if a vector of nucleotide sequences are coding to the same sequence.
#'
#' @param seqs A vector of nucleotide sequences.
#' @return A logical value indicating whether all input sequences are coding to the same sequence.
#' @examples
#' coding_to_same_seq(c("ATG", "ATG"))
#'   #> TRUE
#' coding_to_same_seq(c("ATG", "ATC"))
#'   #> FALSE
#'
coding_to_same_seq <- function(seqs) {
  # Check if there are at least two sequences in the input vector.
  if (length(seqs) < 2) return(TRUE)

  # Compare the first sequence to the rest of the sequences in the vector.
  for (seq in seqs[-1]) {
    if (!compare_codon_indexes(seqs[[1]], seq)) return(FALSE)
  }

  # If all comparisons were successful, return TRUE.
  return(TRUE)
}

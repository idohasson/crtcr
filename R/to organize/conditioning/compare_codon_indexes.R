#' Compare two nucleotide sequences by their codon indexes.
#'
#' @param seq1 A nucleotide sequence.
#' @param seq2 A nucleotide sequence.
#' @return A logical value indicating whether the two input nucleotide sequences
#'   have the same codon index value.
#' @examples
#' compare_codon_indexes("ATG", "ATG")
#'   #> TRUE
#' compare_codon_indexes("ATG", "ATC")
#'   #> FALSE
#'
compare_codon_indexes <- function(seq1, seq2) {
  all(nt2aa(seq1) == nt2aa(seq2))
}



#' Calculate the aa_index value for a given amino acid sequence.
#'
#' @param aa An amino acid sequence.
#' @return The aa_index value for the input amino acid sequence.
#' @examples
#' aa_index("MGR")
#'   #> 917
#'
aa_index <- function(aa) {
  # a vector of integers that correspond to the nucleotide characters.
  base_i <- c(T=0, C=1, A=2, G=3)

  base_i[aa[seq(1,length(aa),3)]] * 16 +
    base_i[aa[seq(2,length(aa),3)]] * 4 +
    base_i[aa[seq(3,length(aa),3)]] + 1
}

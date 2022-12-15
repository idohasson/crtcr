#' Translate a vector of nucleotide sequences into amino acid sequences.
#'
#' @param nt_vec A vector of nucleotide sequences. Each sequence must contain
#'   consecutive triplets of 'A','G','T','C' (uppercase)
#' @return A vector of amino acid sequences.
#' @examples
#' translate(c("ATGAGACCCAGG", "ATGAGACCCAGG", "ATGAGACCCAGG"))
#'   #> "MGRMGRMGR"
#'
translate <- function(nt_vec) {
  # checks that each element in the vector is a DNA base.
  stopifnot("All strings must have consecutive triplets of 'A','G','T','C' (uppercase)"=all(grepl(pattern = "^([AGTC]{3})+$", nt_vec)))
  # function for converting a DNA character vector into an amino acid character vector.
  nt_to_aa <- function(nt) {
    # a vector of integers that correspond to the nucleotide characters.
    base_i <- c(T=0, C=1, A=2, G=3)
    # a vector of characters that correspond to the amino acid characters.
    codon_talbe <- c("F", "F", "L", "L", "S", "S", "S", "S",
                     "Y", "Y", "*", "*", "C", "C", "*", "W",
                     "L", "L", "L", "L", "P", "P", "P", "P",
                     "H", "H", "Q", "Q", "R", "R", "R", "R",
                     "I", "I", "I", "M", "T", "T", "T", "T",
                     "N", "N", "K", "K", "S", "S", "R", "R",
                     "V", "V", "V", "V", "A", "A", "A", "A",
                     "D", "D", "E", "E", "G", "G", "G", "G")

    aa_index <- base_i[nt[seq(1,length(nt),3)]] * 16 +
      base_i[nt[seq(2,length(nt),3)]] * 4 +
      base_i[nt[seq(3,length(nt),3)]] + 1

    paste(codon_talbe[aa_index], collapse="")

  }

  vapply(strsplit(nt_vec, NULL), nt_to_aa, character(1))
}

#' Translate a vector of nucleotide sequences into amino acid sequences.
#'
#' @param nt_vec A vector of nucleotide sequences. Each sequence must contain
#'   consecutive triplets of 'A','G','T','C' (uppercase)
#' @return A vector of amino acid sequences.
#' @examples
#' translate(c("ATGAGACCCAGG", "ATGAGACCCAGG", "ATGAGACCCAGG"))
#'   #> "MGRMGRMGR"
#'
translate <- function(nt_vec) {
  # checks that each element in the vector is a DNA base.
  stopifnot("All strings must have consecutive triplets of 'A','G','T','C' (uppercase)"=all(grepl(pattern = "^([AGTC]{3})+$", nt_vec)))

  vapply(strsplit(nt_vec, NULL), nt_to_aa, character(1))
}

#' Convert a nucleotide sequence into an amino acid sequence.
#'
#' @param nt A nucleotide sequence. The sequence must contain consecutive
#'   triplets of 'A','G','T','C' (uppercase)
#' @return An amino acid sequence.
#' @examples
#' nt_to_aa("ATGAGACCCAGG")
#'   #> "MGR"
#'
nt_to_aa <- function(nt) {
  # a vector of integers that correspond to the nucleotide characters.
  base_i <- c(T=0, C=1, A=2, G=3)
  # a vector of characters that correspond to the amino acid characters.
  codon_talbe <- c("F", "F", "L", "L", "S", "S", "S", "S",
                   "Y", "Y", "*", "*", "C", "C", "*", "W",
                   "L", "L", "L", "L", "P", "P", "P", "P",
                   "H", "H", "Q", "Q", "R", "R", "R", "R",
                   "I", "I", "I", "M", "T", "T", "T", "T",
                   "N", "N", "K", "K", "S", "S", "R", "R",
                   "V", "V", "V", "V", "A", "A", "A", "A",
                   "D", "D", "E", "E", "G", "G", "G", "G")

  aa_index <- base_i[nt[seq(1,length(nt),3)]] * 16 +
    base_i[nt[seq(2,length(nt),3)]] * 4 +
    base_i[nt[seq(3,length(nt),3)]] + 1

  paste(codon_talbe[aa_index], collapse="")

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

#' Compare two amino acid sequences by their aa_index values.
#'
#' @param aa1 An amino acid sequence.
#' @param aa2 An amino acid sequence.
#' @return A logical value indicating whether the two input amino acid sequences
#'   have the same aa_index value.
#' @examples
#' compare_aa_sequences("MGR", "MGR")
#'   #> TRUE
#' compare_aa_sequences("MGR", "AGR")
#'   #> FALSE
#'
compare_aa_sequences <- function(aa1, aa2) {
  aa_index1 <- aa_index(aa1)
  aa_index2 <- aa_index(aa2)

  aa_index1 == aa_index2
}



#' Check whether all nucleotide sequences in a character vector are equal.
#'
#' @param nt_vec A vector of nucleotide sequences. Each sequence must contain
#'   consecutive triplets of 'A','G','T','C' (uppercase)
#' @return A logical value indicating whether all nucleotide sequences in the
#'   input vector are equal.
#' @examples
#' check_nt_sequence_equality(c("ATGAGACCCAGG", "ATGAGACCCAGG", "ATGAGACCCAGG"))
#'   #> TRUE
#' check_nt_sequence_equality(c("ATGAGACCCAGG", "ATGAGACCCAGG", "ATGAAACCCAGG"))
#'   #> FALSE
#'
check_nt_sequence_equality <- function(nt_vec) {
  # split each nucleotide sequence into its component triplets
  split_nt_vec <- strsplit(nt_vec, NULL)

  # apply the compare_aa_sequences function to all pairs of nucleotide sequences
  Reduce(function(x, y) x & compare_aa_sequences(x, y), split_nt_vec)
}


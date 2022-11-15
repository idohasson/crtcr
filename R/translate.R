#' Converts nucleotide sequence to amino acid sequence
#'
#' @param nt_vec DNA sequences as character vector composed with the letters
#' "A", "G", "T" or"C" (case-sensitive to upper case).
#'
#' @return Character vector of the coding amino acid sequences.
#' @export
#'
#' @examples
#'
#' translate(c("AGTTTA", "ATGCCT", "GGTTGAAAA")) # asterisk ('*') is a stop codon
#'
translate <- function(nt_vec) {
  # checks that each element in the vector is a DNA base.
  stopifnot(all(grepl(pattern = "^[AGTC]+$", nt_vec)))
  # a vector of integers that correspond to the nucleotide characters.
  encoding <- c(T = 0, C = 1, A = 2, G = 3)
  # a vector of characters that correspond to the amino acid characters.
  decoding <- strsplit("FFLLSSSSYY**CC*WLLLLPPPPHHQQRRRRIIIMTTTTNNKKSSRRVVVVAAAADDEEGGGG", "")[[1]]
  # function for converting a DNA character vector into an amino acid character vector.
  nt_to_aa <- function(nt) decoding[encoding[nt[seq(1, length(nt) ,3)]] * 16 +
                                      encoding[nt[seq(2, length(nt) ,3)]] * 4 +
                                      encoding[nt[seq(3, length(nt) ,3)]] + 1]
  # splits the nucleotide vector into a character vector of individual nucleotides
  nt_vec <- strsplit(nt_vec, split = "")
  # converts each nucleotide character to an amino acid character.
  aa_vec <- lapply(nt_vec, nt_to_aa)
  # pastes the amino acid characters together into a single string.
  sapply(aa_vec, paste, collapse="")
}

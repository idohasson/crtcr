#' Converts nucleotides to amino acids
#'
#' @description
#' Converts a character vector of nucleotide sequences (uppercase 'A','G','T','C')
#' to their corresponding amino acid sequences by decoding consecutive triplets
#' with the codon table.
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




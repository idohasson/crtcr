#' Convert a vector of nucleotide sequences into a vector of amino acid sequences.
#'
#' @param nt_vec A vector of nucleotide sequences. Each sequence must contain consecutive
#'   triplets of 'A','G','T','C' (uppercase)
#' @return A vector of amino acid sequences.
#' @examples
#' translate(c("ATGAGACCCAGG", "ATGAGACCCAGG"))
#'   #> c("MGR", "MGR")
#'
translate <- function(nt_vec) {
  # Check that each element in the vector is a DNA base.
  if (all(grepl(pattern = "^([AGTC]{3})+$", sequences))) stop(

    "All strings must have consecutive triplets of 'A','G','T','C' (uppercase)"

  )

  # Split the input vector into individual sequences.
  sequences <- strsplit(nt_vec, NULL)

  # Apply the nt_to_aa function to each sequence.
  vapply(sequences, nt_to_aa, character(1))
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










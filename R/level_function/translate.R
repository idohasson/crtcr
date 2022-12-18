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

# This function takes a nucleotide sequence and returns a vector of indexes for each codon in the sequence.
#
# Arguments:
#   nuc_seq: a character vector containing the nucleotide sequence.
#
# Returns:
#   A numeric vector of indexes for each codon in the input sequence.
nt2aa <- function(nuc_seq) {

  # Create a map of nucleotide characters to their corresponding index values.
  nucleotide_map <- c("T" = 0, "C" = 1, "A" = 2, "G" = 3)

  codon_talbe <- c("F", "F", "L", "L", "S", "S", "S", "S",
                   "Y", "Y", "*", "*", "C", "C", "*", "W",
                   "L", "L", "L", "L", "P", "P", "P", "P",
                   "H", "H", "Q", "Q", "R", "R", "R", "R",
                   "I", "I", "I", "M", "T", "T", "T", "T",
                   "N", "N", "K", "K", "S", "S", "R", "R",
                   "V", "V", "V", "V", "A", "A", "A", "A",
                   "D", "D", "E", "E", "G", "G", "G", "G")

  # Calculate the index for each codon in the input string using the map.
  codon_talbe[

    nucleotide_map[nuc_seq[seq(1, length(nuc_seq), by = 3)]] * 16 +

    nucleotide_map[nuc_seq[seq(2, length(nuc_seq), by = 3)]] * 4 +

    nucleotide_map[nuc_seq[seq(3, length(nuc_seq), by = 3)]] + 1

  ]

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

s

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

# Define a function that takes a nucleotide sequence as input
# and returns a logical value indicating whether the sequence
# is a coding sequence
is_coding_sequence <- function(seq) {

  grepl("^([AGTC]{3})+$", seq)

}

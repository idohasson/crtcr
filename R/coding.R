# Define a function that takes a nucleotide sequence as input
# and returns a logical value indicating whether the sequence
# is a coding sequence
is_coding <- function(nucleotide) {

  grepl("^([AGTC]{3})+$", nucleotide)

}

all_coding <- function(nucleotide) {

  all(is_coding(nucleotide))

}

check_coding <- function(nucleotide) {

  if (!all_coding(nucleotide)) stop(

    "Only coding sequences are supported. All strings must have consecutive triplets of 'A','G','T','C' (uppercase)"

  )

}

#' Convert a vector of nucleotide sequences into a vector of amino acid sequences.
#'
#' @param nt_vec A vector of nucleotide sequences. Each sequence must contain consecutive
#'   triplets of 'A','G','T','C' (uppercase)
#' @return A vector of amino acid sequences.
#' @examples
#' translate(c("ATGAGACCCAGG", "ATGAGACCCAGG"))
#'   #> c("MGR", "MGR")
#'
convert_sequence <- function(nt_vec) {
  # Check that each element in the vector is a DNA base.
  check_coding(nt_vec)

  sequences <- lapply(strsplit(nt_vec, NULL), dna_to_protein)

  vapply(sequences, paste, character(1), collapse="")

}

# This function takes a nucleotide sequence and returns a vector of indexes for each codon in the sequence.
#
# Arguments:
#   nuc_seq: a character vector containing the nucleotide sequence.
#
# Returns:
#   A numeric vector of indexes for each codon in the input sequence.
dna_to_protein <- function(nuc_seq) {

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




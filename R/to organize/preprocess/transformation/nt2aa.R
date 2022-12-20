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


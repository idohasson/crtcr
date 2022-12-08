
codon_table <- c(

  "TTT", "TTC", "TTA", "TTG", "CTT", "CTC", "CTA", "CTG",
  "ATT", "ATC", "ATA", "ATG", "GTT", "GTC", "GTA", "GTG",
  "TCT", "TCC", "TCA", "TCG", "CCT", "CCC", "CCA", "CCG",
  "ACT", "ACC", "ACA", "ACG", "GCT", "GCC", "GCA", "GCG",
  "TAT", "TAC", "TAA", "TAG", "CAT", "CAC", "CAA", "CAG",
  "AAT", "AAC", "AAA", "AAG", "GAT", "GAC", "GAA", "GAG",
  "TGT", "TGC", "TGA", "TGG", "CGT", "CGC", "CGA", "CGG",
  "AGT", "AGC", "AGA", "AGG", "GGT", "GGC", "GGA", "GGG",

  "F", "F", "L", "L", "L", "L", "L", "L",
  "I", "I", "I", "M", "V", "V", "V", "V",
  "S", "S", "S", "S", "P", "P", "P", "P",
  "T", "T", "T", "T", "A", "A", "A", "A",
  "Y", "Y", "*", "*", "H", "H", "Q", "Q",
  "N", "N", "K", "K", "D", "D", "E", "E",
  "C", "C", "*", "W", "R", "R", "R", "R",
  "S", "S", "R", "R", "G", "G", "G", "G"

)
codon_array <- array(codon_table, c(4,4,4,2))


codon_talbe <- c("F", "F", "L", "L", "S", "S", "S", "S",
                 "Y", "Y", "*", "*", "C", "C", "*", "W",
                 "L", "L", "L", "L", "P", "P", "P", "P",
                 "H", "H", "Q", "Q", "R", "R", "R", "R",
                 "I", "I", "I", "M", "T", "T", "T", "T",
                 "N", "N", "K", "K", "S", "S", "R", "R",
                 "V", "V", "V", "V", "A", "A", "A", "A",
                 "D", "D", "E", "E", "G", "G", "G", "G")

NT_AXIS <- c(T=0L, C=1L, A=2L, G=3L)
TABLE_INDEX <- c(1L, 1L, 2L, 2L, 3L, 3L, 3L, 3L,
                 4L, 4L, 5L, 5L, 6L, 6L, 5L, 7L,
                 2L, 2L, 2L, 2L, 8L, 8L, 8L, 8L,
                 9L, 9L, 10L,10L,11L,11L,11L,11L,
                 12L,12L,12L,13L,14L,14L,14L,14L,
                 15L,15L,16L,16L, 3L, 3L,11L,11L,
                 17L,17L,17L,17L,18L,18L,18L,18L,
                 19L,19L,20L,20L,21L,21L,21L,21L)

nucleotide_axis <- function(nucleotide) {

  vapply(nucleotide, function(nt) index[nt], integer(1), USE.NAMES = FALSE)

}

triplet_code <- function(three_bases) {

  three_bases[1] * 16L + three_bases[2] * 4L + three_bases[3] + 1L

}

axis_vec <- nucleotide_axis(nt_vec)

codon_list <- split(axis_vec, gl(length(axis_vec) %/% 3 ,3))

vapply(codon_list, triplet_code, integer(1), USE.NAMES = FALSE)



# Define the nucleotide sequences
nucleotides <- c("ATGCTAGCAT", "ATGCTAGCAT", "ATGCTAGCAT", "ATGCTAGCAA", "ATGCTAGCAG")

# Define the reference protein sequence
reference <- "MCTAC"

aa_seq <- unlist(strsplit(reference, ""))
aa_seq


# Align the nucleotide sequences to the reference protein sequence
aligned <- nucleotides[match(reference, nucleotides)]








# CODON <- matrix(, 64, 2)

translate <- function(nt_str) {


  paste_vec <- function(tbl_indices) {

    aa_vac <- codon_talbe[tbl_indices]

    paste(aa_vac, collapse = "")

  }

  nt_vec <- strsplit(nt_str, "")

  codon_indices <- lapply(nt_vec, get_codon_indices)

  vapply(codon_indices, paste_vec, character(1L))

}


get_codon_indices <- function(nt_vec, index=c(T=0L, C=1L, A=2L, G=3L)) {

  nucleotide_axis <- function(nucleotide) {

    vapply(nucleotide, function(nt) index[nt], integer(1), USE.NAMES = FALSE)

  }

  triplet_code <- function(three_bases) {

    three_bases[1] * 16L + three_bases[2] * 4L + three_bases[3] + 1L

  }

  axis_vec <- nucleotide_axis(nt_vec)

  codon_list <- split(axis_vec, gl(length(axis_vec) %/% 3 ,3))

  vapply(codon_list, triplet_code, integer(1), USE.NAMES = FALSE)

}


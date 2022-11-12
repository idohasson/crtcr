clonalSeq2DF <- function(clonal_seq) {

  data.frame(clone=clonal_seq, clonotype=translate(clonal_seq))

}

rep2DF <- function(rep, clone_var, clonnotype_var) {

  if (is.character(rep))
    return(clonalSeq2DF(rep))

  clonal_seq <- pull(rep, {{clone_var}})

  if (methods::hasArg(clonnotype_var)) {

    clonotype_seq <- pull(rep, {{clonnotype_var}})

  } else {

    clonotype_seq <- translate(clonal_seq)

  }

  data.frame(clone=clonal_seq, clonotype=clonotype_seq)

}

repList2DF <- function(rep_list, ...) {

  purrr::map_if(rep_list, is.data.frame, rep2DF, ..., .else = clonalSeq2DF) %>%

  # lapply(rep_list, clonalSeq2DF, ...) %>%

    bind_rows(.id = "rid")

}

groupList2DF <- function(group_list, ...) {

  lapply(group_list, repList2DF, ...) %>%

    bind_rows(.id = "gid")
}

translate <- function(nt_vec) {
  # converts the nucleotide vector to uppercase
  # nt_vec <- toupper(nt_vec)
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

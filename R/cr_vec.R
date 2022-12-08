# # First, install and load the vctrs and BioConductor packages
# # install.packages("vctrs")
# install.packages("BiocManager")
# library(vctrs)
# library(BiocManager)
#
# # Use the vec_c() function from the vctrs package to create a vector of nucleotide sequences
# # The vec_c() function allows you to concatenate vectors of different lengths by filling shorter vectors with NA values

#
# # Use the translate() function from the BioConductor package to translate the nucleotide sequences to amino acid sequences
# # The translate() function takes a vector of nucleotide sequences as input and returns a vector of the corresponding amino acid sequences
# aa_seqs <- translate(nuc_seqs)
#
# # Print the resulting vector of amino acid sequences
# print(aa_seqs)


# Load the vctrs package
library(vctrs)
library(tidyr)

# Define a function that takes a nucleotide sequence as input
# and returns a cr class character vector
# new_cr(c("ATGCTA", "AACCGG", "ATGGGA"))
new_cr <- function(x) {
  # Use the new_vctr() function to create a cr class vector
  # of the input sequence
  vec_assert(x, character(), arg = "clonal_seq")

  new_vctr(x, class = "vctrs_cr")

}

# cr(c("ATGCTA", "AACCGG", "ATGGGA"))
cr <- function(x = character()) {
  x <- vec_cast(x, character())
  new_cr(x)
}

# nuc_seqs <- c("ATGCTA", "AACCGG", "ATGGGA")
# is_cr(cr(nuc_seqs))
# is_cr(nuc_seqs)
is_cr <- function(x) {
  inherits(x, "vctrs_cr")
}

nuc_seqs <- c("ATGCTA", "AACCGG", "ATGGGA")
is_cr(nuc_seqs)
is_cr(cr(nuc_seqs))


# check_coding_sequence("ATC")
check_coding_sequence <- function(seq) {

  only_coding_AGTC <- grepl("^([AGTC]{3})+$", seq)

  coding_seq <- all(only_coding_AGTC)

  stopifnot(coding_seq)

}

nt <- replicate(10, rand_nt(1))
aa <- translate(nt)

cr_level <- function(nt, ..., aa=translate(nt), named.aa=FALSE) {

  dfl <- df_list(nt, ..., aa, .name_repair = "universal")

  df <- new_data_frame(dfl)

  cr_seq_list <- split(df[,1], df[,-1])

  # cr_seq <- enframe(split_cr, name = "clonotype", value = "clone")
  # cr_seq_list <- field(cr_seq, "clone")

  cr_level_list <- lapply(cr_seq_list, vec_unique_count)

  cr_level <- unlist(cr_level_list, use.names = named.aa)

  enframe(cr_level, value = "CRlevel")


}


cr_level(nt, rid=rep_along(nt, 1:3), named.aa = T)

  # separate(col = 1, sep = ".")

if (isTRUE(named)) {

  field(cr_seq, "clone") <- cr_level

  cr_level <- deframe(cr_seq)

}

cr_level

# enframe()




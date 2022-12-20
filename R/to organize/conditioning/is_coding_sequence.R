# Define a function that takes a nucleotide sequence as input
# and returns a logical value indicating whether the sequence
# is a coding sequence
is_coding_sequence <- function(seq) {

  grepl("^([AGTC]{3})+$", seq)

}

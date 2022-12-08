# Define a function that takes a nucleotide sequence as input
# and returns a logical value indicating whether the sequence
# is a coding sequence
is_coding_sequence <- function(seq) {

  grepl("^([AGTC]{3})+$", seq)

}

# Test the is_coding_sequence() function
is_coding_sequence("ATCG")
#> FALSE
is_coding_sequence("ATCGATCG")
#> TRUE

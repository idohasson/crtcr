#' Count the number of unique clonal sequences in a nucleotide sequence.
#'
#' This function takes a nucleotide sequence and a corresponding amino acid
#' sequence, and returns the number of unique clonal sequences in the nucleotide
#' sequence. The nucleotide sequence is automatically translated to an amino acid
#' sequence if necessary. The output can be returned as a named or unnamed vector.
#'
#' @param nucleotide a character vector containing the nucleotide sequence.
#' @param amino_acid a character vector containing the corresponding amino acid
#'   sequence. If not provided, the nucleotide sequence will be automatically
#'   translated to an amino acid sequence.
#' @param proportions a logical value indicating whether to return the proportions of
#'   unique clonal sequences instead of their counts.
#' @param ignore_na a logical value indicating whether to remove missing values
#'   before computing the result.
#' @param names a logical value indicating whether to return the result as a
#'   named vector.
#'
#' @return a numeric vector containing the number of unique clonal sequences in
#'   the nucleotide sequence. If `named` is `TRUE`, the result will be a named
#'   vector.
#'
#' @examples
#' nucleotide <- c("ATG", "TGA", "TGA", "TAA", "TAA")
#' amino_acid <- c("M", "W", "W", "*", "*")
#' convergent_recombination_level(nucleotide, amino_acid)
#'   # returns: 2
#'
#' nucleotide <- c("ATG", "TGA", "TGA", "TAA", "TAA")
#' convergent_recombination_level(nucleotide)
#'   # returns: 2
#'
#' @export
convergent_recombination_level <- function(nucleotide, amino_acid, proportions=FALSE, ignore_na = FALSE, names=FALSE) {

  # check if the nucleotide sequence contains only valid nucleotides
  if (any(!grepl("^([AGTC]{3})+$", nucleotide))) stop(
    "Only coding sequences are supported"
  )

  # translate the nucleotide sequence if necessary
  if (rlang::is_missing(amino_acid)) {

    amino_acid <- translate(nucleotide)

  }

  # split the nucleotide sequence into clonal sequences
  clonal_sequeces <- vctrs::vec_split(nucleotide, amino_acid)

  # create a list of unique clonal sequences
  clone_list <- vctrs::field(clonal_sequeces, "val")

  # count the number of unique clonal sequences
  converged_number <- vapply(clone_list, converged_number, numeric(1), proportions, ignore_na)

  # return the number of unique clonal sequences
  if (isTRUE(names)) {
    # return named vector
    vctrs::field(clonal_sequeces, "val") <- converged_number

    return(tibble::deframe(clonal_sequeces))

  } else {
    # return unnamed vector
    return(converged_number)
  }
}

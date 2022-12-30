#' Calculate convergent recombination level by the number of unique nucleotide sequences
#'
#' This function calculates the convergent recombination level of a TCR sequence by counting the number of unique nucleotide sequences in the sequence. The TCR sequence is given as a character vector of DNA sequences in the `.clone` argument.
#'
#' @param nucleotide A character vector of DNA sequences representing the TCR sequence to be analyzed
#' @param ... Additional arguments (currently not used)
#' @return A numeric value indicating the convergent recombination level of the TCR sequence
#' @export
cr_number <- function(nucleotide, ...) {
  unique_count(nucleotide, ...)
}

cr_mean <- function(nucleotide, ...)  {
  unique_mean(nucleotide)
}

cr_level <- function(x, ..., .cr_func=cr_number) {

  dfl <- df_list(nt=x, ..., .name_repair = "minimal")
  # return(dfl)
  if (vec_size(dfl) == 0)

    return(NA)

  else if (vec_size(dfl) == 1)

    return(.share_func(dfl$nt))

  tapply(dfl$nt, dfl[-1], .cr_func, default = 0)

}

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
#' cr_level(nucleotide, amino_acid)
#'   # returns: 2
#'
#' nucleotide <- c("ATG", "TGA", "TGA", "TAA", "TAA")
#' cr_level(nucleotide)
#'   # returns: 2
#'
#' @export
cr_level <- function(nucleotide, amino_acid, proportions=FALSE, ignore_na = FALSE, names=FALSE) {

  # translate the nucleotide sequence if necessary
  if (rlang::is_missing(amino_acid)) {

    amino_acid <- convert_sequence(nucleotide)

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

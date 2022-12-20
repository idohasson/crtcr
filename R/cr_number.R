#' Calculate convergent recombination level by the number of unique nucleotide sequences
#'
#' This function calculates the convergent recombination level of a TCR sequence by counting the number of unique nucleotide sequences in the sequence. The TCR sequence is given as a character vector of DNA sequences in the `.clone` argument.
#'
#' @param nucleotide A character vector of DNA sequences representing the TCR sequence to be analyzed
#' @param ... Additional arguments (currently not used)
#' @return A numeric value indicating the convergent recombination level of the TCR sequence
#' @export
cr_number <- function(nucleotide, ...) {
  UseMethod("cr_number")
}

# cr_number(nt)
cr_number.character <- function(nucleotide, ...) {

  nt_list <- dots_splice(nucleotide, ...)

  nucleotide <- rlang::squash_chr(nt_list)

  # vapply(nt_list, vec_unique_count, integer(1))
  vapply(nt_list, vec_unique_count, integer(1), ...)

}

# cr_number(split(nt, aa))
cr_number.list <- function(nucleotide, ...) {

  nt_list <- dots_splice(nucleotide, ...)

  lapply(rlang::squash(nt_list), cr_number, ...)

}

# cr_number(df, col_index="nt")
cr_number.data.frame <- function(nucleotide, ..., col_index=1) {

  nucleotide <- vctrs::field(nucleotide, col_index)

  cr_number(nucleotide, ...)

}

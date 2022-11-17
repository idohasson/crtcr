cr_level <- function(clonal_sequnces, .func, ..., check_unique=) {
  # share_level.default
  n_distinct(..., na.rm = TRUE)
}

cr_level.character <- function()

#' Convergent recombination level (CR-level) of clonal sequences
#'
#' @description
#' calculate unique number of nucleotide sequences encoding a specific amino
#' acid sequence. This value associated with TCR repertoire clonal sequences
#' and termed as "CR-level" (convergent recombination level).
#'
#' @param ... nucleotide sequences
#' @param always_named name single value. Default `TRUE`.
#'
#' @return CR-level integer vector (named by the clonotype)
cr_level_vec <- function(clonal_seq, func=translate, ...) {

  apply_func <- function(.var)
    eval(as.call(list(func, quote(.var))))

  tapply(clonal_seq, translate(clonal_seq), n_distinct)

  # if (always_named | !testScalar(clonal_sequence))
  #
  #   return(clonal_sequence)
  #
  # else
  #
  #   return (unname(clonal_sequence))

}



get_clonotype <- function(.data, func, ..., attr_vars) {

}

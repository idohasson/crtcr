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
cr_level_vec <- function(clonal_seq, always_named=FALSE) {

  tapply(clonal_seq, translate(clonal_seq), n_distinct)

  # if (always_named | !testScalar(clonal_sequence))
  #
  #   return(clonal_sequence)
  #
  # else
  #
  #   return (unname(clonal_sequence))

}

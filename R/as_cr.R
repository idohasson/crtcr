# library(bioseq)
# library(rlang)
# library(vctrs)

#' Convert TCR sequence to index list using convergent recombination sequences
#'
#' This function converts a TCR sequence, given as a character vector of DNA sequences in `.clone`, to an index list using the convergent recombination sequences. The DNA sequences must be translated and have the right corresponding codon sequence.
#'
#' @param .clone A character vector of DNA sequences representing the TCR sequence to be converted
#' @param is_nt_clone A logical value indicating whether the input DNA sequences are in nucleotide form (`TRUE`) or amino acid form (`FALSE`). Default is `TRUE`.
#' @return A list of indexes representing the TCR sequence
#' @export
as_cr <- function(.clone, is_nt_clone=FALSE) {

  clonotype <- cr_seq(.clone)

  clonotype_group <- vec_group_loc(clonotype)

  # if (is_cid(.clone))
  #
  #   clonotype_group <- .clone
  #
  # else if (isTRUE(is_nt_clone) | all_nt_clone(.clone)) {
  #
  #   clonotype <- translate(.clone)
  #
  #   clonotype_group <- vec_group_loc(clonotype)
  #
  # } else if (all_aa_clone(.clone))
  #
  #   clonotype_group <- vec_group_loc(.clone)
  #
  # else stop(.clone, "must be aa or nt vector")

  new_list_of(clonotype_group$loc, ptype = integer(), class = "cr")

}

cr_seq <- function(.clone) {

  if (all_aa_clone(.clone))

    return(.clone)

  translate(.clone)

}


get_cid <- function(.clone, ...) {

  if (all_nt_clone(.clone))

    .clone <- translate(.clone)

  vec_group_id(.clone)

  # if (is_cid(.clone))
  #
  #   return(.clone)
  #
  # else if (all_nt_clone(.clone))
  #
  #   clonotype <- translate(.clone)
  #
  # else if (all_aa_clone(.clone))
  #
  #   clonotype <- .clone
  #
  # else stop("must be aa or nt vector")
  #
  #
  # vec_group_id(clonotype)

    # clonotype_group <- vec_group_loc(.clone)

  # vec_duplicate_id(translate(xx))




  # cr_list <- vec_split(clone_id, clonotype)
  #
  # deframe(clone_id)


  # cid <- vec_group_id(clonotype)
  #
  # attr(cid, "clonotype") <- vec_unique(clonotype)
  #
  # cid
}


is_cid <- function(.clone) {

  if ("cr" %in% class(.clone))

    return(TRUE)

  else if (is_list(.clone))

    return(all(sapply(my_list, is.integer)))

  else

    return(FALSE)

}

all_aa_clone <- function(.clone) {

  if (is_character(.clone))

    return(FALSE)

  all(grepl("^[RKGSVLICPWNTDHAQFYEM*-]+$", .clone))

}

all_nt_clone <- function(.clone) {

  if (is_character(.clone))

    return(FALSE)

  all(grepl("^([AGTC]{3})+$", .clone))

}



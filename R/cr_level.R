
# number of unique nucleotide sequences.

with(x, convergent_recombination_level(nt, aa))
#' Calculate the convergent recombination level of a nucleotide sequence
#'
#' This function calculates the convergent recombination level of a nucleotide sequence by splitting the sequence into clonal sequences and counting the number of unique clonal sequences.
#'
#' @param nucleotide A character vector containing the nucleotide sequence.
#' @param amino_acid An optional character vector containing the amino acid sequence. If this is not provided, the function will translate the nucleotide sequence.
#' @param prop A logical value indicating whether the convergent recombination level should be calculated as a proportion of the total number of clonal sequences.
#' @param na.rm A logical value indicating whether NA values should be removed before calculating the convergent recombination level.
#' @param named A logical value indicating whether the result should be returned as a named vector.
#' @return A numeric vector containing the convergent recombination level for each clonal sequence. If the `named` argument is `TRUE`, the result will be a tibble data frame with the clonal sequence names as the row names.
#' @export
#' @examples
#' convergent_recombination_level(nucleotide = "ATCGATCG")
#' convergent_recombination_level(nucleotide = "ATCGATCG", amino_acid = "IK")
convergent_recombination_level <- function(nucleotide, amino_acid, prop=FALSE, na.rm = FALSE, named=FALSE) {

  if (any(!grepl("^([AGTC]{3})+$", nucleotide))) stop(

    "Only coding sequences are supported"

  )

  if (rlang::is_missing(amino_acid)) {
    # translates the nucleotide sequence
    amino_acid <- translate(nucleotide)
  }
  clonal_sequeces <- vctrs::vec_split(nucleotide, amino_acid)
  # splits the nucleotide sequence into clonal sequences
  clone_list <- vctrs::field(clonal_sequeces, "val")
  # counts the number of unique clonal sequences
  converged_number <- vapply(clone_list, converged_number, numeric(1), prop, na.rm)
  # returns the number of unique clonal sequences
  if (isTRUE(named)) {
    vctrs::field(clonal_sequeces, "val") <- converged_number
    # returns named vector
    return(tibble::deframe(clonal_sequeces))
  } else {
    # return unnamed vector
    return(converged_number)
  }
}

#' Calculate the number of unique elements in a vector.
#'
#' This function takes a vector `nucleotide` and calculates the number of unique elements in the vector. By default, the function returns the number of unique elements as an integer. If the `prop` argument is set to `TRUE`, the function returns the proportion of unique elements in the vector instead. If the `na.rm` argument is set to `TRUE`, the function will remove any `NA` values from the vector before calculating the number of unique elements.
#'
#' @param nucleotide A vector of nucleotide sequences.
#' @param prop A logical value indicating whether to return the proportion of unique elements in the vector (default is `FALSE`).
#' @param na.rm A logical value indicating whether to remove `NA` values from the vector before calculating the number of unique elements (default is `FALSE`).
#' @return An integer or numeric value indicating the number or proportion of unique elements in the vector.
#' @examples
#' # Generate some random nucleotide sequences
#' nucleotide <- c("A", "T", "G", "C", "A", "T", "G", "C", "A", "T", "G", "C")
#'
#' # Calculate the number of unique nucleotide sequences
#' converged_
converged_number <- function(nucleotide, prop=FALSE, na.rm = FALSE) {

  is_unique_clone <- !duplicated(nucleotide)

  if (isTRUE(na.rm)) {

    non_na <- vctrs::vec_detect_complete(nucleotide)

    is_unique_clone <- non_na & is_unique_clone

  }

  converge_count <- sum(is_unique_clone)

  if (isTRUE(prop)) {

    return(converge_count / vec_size(is_unique_clone))

  } else {

    return(converge_count)

  }

}


convergence_level <- function(nucleotide) {

  is_unique_clone <- !duplicated(nucleotide)

  clonotype <- translate(nucleotide[nucleotide])

  convergent_number <- vec_count(clonotype, sort = "none")

  field(convergent_number, "count")

}


prepare <- function(nucleotide, amino_acid, ...) {

  if (missing(amino_acid))

    amino_acid <- translate(nucleotide)

  dfl <- df_list(.clone=nucleotide, .clontype=amino_acid, ..., .name_repair = "minimal")

  new_data_frame(dfl, class = "cr")

}

convergent_recombination_level <- function(nucleotide, amino_acid, ...) {

  df <- prepare(nucleotide, amino_acid, ...)

  unique_clones <- vec_unique(df)

  converged_clone_count <- vec_count(unique_clones[-1], sort = "location")

  field(converged_clone_count, "count")

}





# convergent_level <-
#
#   if (isTRUE(unnamed)) {
#     # returns the number of unique clonal sequences
#     return(convergent_level)
#
#   } else {
#     # returns the number of unique clonal sequences
#     vctrs::field(clonal_sequeces, "val") <- convergent_level
#     # named by the clonotype sequence
#     named_by_clonotype <- tibble::deframe(clonal_sequeces)
#
#     return(named_by_clonotype)
#
#   }



converged_clones <- function(nucleotide, amino_acid, unnamed=TRUE) {

  clonotype_split <- vec_split(nucleotide, amino_acid)

  clone_list <- field(clonotype_split, "val")

  unique_clone_list <- lapply(clonotype_split$val, vec_unique)

  if (isTRUE(unnamed))

    return(unique_clone_list)

  field(clonotype_split, "val") <- unique_clone_list

  tibble::deframe(clonotype_split)

}

# v <- rand_nt_vec()
# cr_level(v)
# clone <- rand_clone_list()
# cr_level(clone)
# cr_level(split(v, gl(3,1,length(v))))

# group unique sequences by their amino acid sequence (CR-sequences)



# Input: nucleotide sequence vector or list of sequences vectoer
cr_level <- function(nt) {

  vctrs::vec_unique_count(nt)

}

cr_level.clonotype <- function(clone_vec) {

  if (check_cr_sequences(clone_vec))

  cr_level(clonotype)

  # clonotype <- get_clonotype(clone_list)

  # vapply(clonotype, cr_level, integer(1))

}

cr_level.clonotype.list <- function(nt_list) {

  vapply(nt_list, length, integer(1), USE.NAMES = FALSE)

}

cr_level.repertoire <- function(clonotype_list) {

  vapply(clonotype_list, cr_level, integer(1))

}
cr_level.repertoire.group <- function(clonotype_list) {

  vapply(clonotype_list, cr_level, integer(1))

}
cr_level.group <- function(clonotype_list) {

  vapply(clonotype_list, cr_level, integer(1))

}


is_clone <- function(vec_list) {

  is.vec <- vapply(vec_list, is_character, logical(1))
  # lapply(nt_list, as.character)
  # seqs <- lapply(list(...), is_scalar_character)
  # seqs
  # as.character(...)
  # x1 <- vapply(xx, length, numeric(1)) > 0
  # vapply(clone_vec, is_vector, logical(1), USE.NAMES = FALSE)
}
get_clone <- function(...) {

  squash_chr(list(...))

}

is_clonotype <- function(nt, aa=translate(nt[1])) {

  if (length(nt)==1)

    return(TRUE)

  for (nt_seq in nt[-1])

    if (aa != translate(nt_seq))

      return(FALSE)

  return(TRUE)

}
get_clonotype <- function(clone) {

  translate(clone)

}

cr_seq <- function(nt) {

  is_unique_nt <- !duplicated(nt)

  nt[unique_nt]

}
cr_sequences <- function(nt) {

  if (check_cr_sequences(nt))

    return(nt)

  else

    return(unique(nt))

}
check_cr_sequences <- function(nt) {

  if (!is_clonotype(nt))

    return(FALSE)

}
get_cr_sequences(nt) {

  if (is_list(nt))

    nt <- unlist(nt, use.names = FALSE)

  nt <- unique(nt)

  split(nt, translate(nt))

}



# cr_level.list <- function(..., each_vector=FALSE) {
#
#   vec_list <- dots_list(..., .preserve_empty = TRUE)
#
#   is.vec <- vapply(vec_list, is_character, logical(1))
#
#   seq_list <- vec_list[which(is.vec)]
#
#   char_vec <- squash_chr(seq_list)
#
#   vec_unique_count(char_vec)
#
# }




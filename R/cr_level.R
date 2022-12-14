cr_level <- function(...) {
  UseMethod("cr_level")
}


cr_level.default <- function(...) {

  sapply(list(...), class)


}


is_cr(c("TTT"))
sapply(rand_clone_list(), is_cr)
is_cr <- function(clone) {

  clonotype <- translate(clone[1])

  for (nt in clone[-1]) {

    if (clonotype != translate(nt)) return(FALSE)

  }

  return(TRUE)


}


cr_seq <- function(nt) {

  is_unique_nt <- !duplicated(nt)

  nt[unique_nt]

}


cr_level.character <- function(nt, ..., is_cr=is_cr(cr_seq),

                               .cr_seq=NULL, each_vector=FALSE) {




  clones <- get_clone(nt, ...)

  clonotypes <- get_clonotype(clones)

  clone_list <- split(clones, clonotypes)

  vapply(clone_list, vec_unique_count, integer(1))

}


get_clone <- function(...) {

  squash_chr(list(...))

}

get_clonotype <- function(clone) {

  translate(clone)

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


is_clone <- function(vec_list) {

  is.vec <- vapply(vec_list, is_character, logical(1))



  # lapply(nt_list, as.character)

  # seqs <- lapply(list(...), is_scalar_character)

  # seqs

  # as.character(...)



  # x1 <- vapply(xx, length, numeric(1)) > 0
  # vapply(clone_vec, is_vector, logical(1), USE.NAMES = FALSE)
}

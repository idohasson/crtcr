
cr_level <- function(..., group_level=mean) {

  levels <- cr_number(...)

  levels

}

cr_level <- function(nt, aa=NULL, named=FALSE) {

  cr_seq <- cr_list(nt, aa, named)

  vapply(cr_seq, vec_unique_count, integer(1))

}

cr_level <- function(nt, ...) {

  nt_list <- dots_splice(nt, ...)

  vapply(nt_list, vec_unique_count, integer(1))

}

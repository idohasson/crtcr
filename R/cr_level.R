#' Convergent recombination level calculation
#'
#' @param ... character vector(s) of the clonal sequences.
#' @param ignore.na if TRUE (default), NA values are not included in the count.
#'
#' @return integer of unique count.
#' @export
#'
#' @examples
#'
#' cr_level(rep(LETTERS[1:2], each=4), gl(2,2,8), gl(4,3,8))
#'
cr_level <- function(clones, ignore.na=TRUE) {

  # clone_lists <- list2(...)
  #
  # clonal_sequences <- squash_chr(clone_lists)

  if (isTRUE(ignore.na)) {

    no_na <- vec_detect_complete(clones)

    clonal_sequences <- clones[no_na]

  }

  vec_unique_count(clones)

}



cr_number <- function(.clone, .relative_cr=FALSE) {

  cr_n <- n_unique(.clone)

  if (isTRUE(.relative_cr)) cr_n <- cr_n / vec_size(.clone)

  cr_n

}

cr_func <- function(.clone, .func=cr_number, ...) {

  lvl <- .func(.clone)

  if (isTRUE(relative))

    lvl <- lvl/vec_size(.clone)

  lvl

}

cr_level <- function(.clone, ..., .cr_func=n_unique) {

  if (rlang::dots_n(...)==0)

    return(.cr_func(.clone))


  .id <- rlang::dots_splice(...)

  tapply(.clone, .id, .cr_func)

}



cr_number <- function(..., na.rm=FALSE) {

  clones_list <- list_of(..., .ptype = character(1L))

  clones <- squash_chr(clones_list)

  if (isTRUE(ignore.na)) {

    no_na <- vec_detect_complete(clones)

    clonal_sequences <- clones[no_na]

  }

  if(isTRUE(na.rm))

    clones <- clones[vec_detect_complete(clones)]

  vec_unique_count(clones)

}

cr_mean <- function(.clone) {

  cr_number(.clone) / vec_size(.clone)

}

cr_pairwise <- function(.clone1, .clone2., .cr_func=cr_number, ...) {
  # TODO: complete
  # https://github.com/matsengrp/sumrep/blob/master/R/Compare.R
  mapply(.cr_func, .clone1, .clone2., ...)

}

cr_group <- function(.clone, .id, ...) {
  # TODO: complete
}

cr_level <- function(.clone, ..., .cr_func=cr_number) {
  # check input
  dfl <- dots_splice(..., .name_repair = "minimal")

  group_df <- new_data_frame(dfl)

  groups <- vec_group_loc(group_df)

  # TODO: new_list_of(groups$loc, ptype = integer())

  clone_list <- vec_chop(.clone, groups$loc)

  vapply(clone_list, .cr_func, numeric(1L), USE.NAMES = FALSE)

}

cr_level_df <- function(.clone, ..., .cr_func=cr_number) {

  dfl <- dots_splice(..., .name_repair = "minimal")

  group_df <- new_data_frame(dfl)

  groups <- vec_group_loc(group_df)

  # TODO: new_list_of(groups$loc, ptype = integer())

  clone_list <- vec_chop(.clone, groups$loc)

  lvl <- vapply(clone_list, .cr_func, numeric(1L), USE.NAMES = FALSE)

  vec_cbind(groups$key, CRlevel=lvl)

}

cr_level_vec <- function(.clone, ..., .cr_func=cr_number) {

  l <- dots_splice(...)

  clone_list <- split(.clone, l)

  vapply(clone_list, .cr_func, numeric(1L), USE.NAMES = FALSE)

}

cr_level_tbl <- function(.clone, ..., .cr_func=cr_number) {

  dfl <- df_list(y,z,.name_repair = "minimal")

  tapply(.clone, dfl, .cr_func)

}

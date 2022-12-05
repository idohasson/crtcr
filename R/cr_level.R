#' Calculate convergent recombination level by the number of unique nucleotide sequences
#'
#' This function calculates the convergent recombination level of a TCR sequence by counting the number of unique nucleotide sequences in the sequence. The TCR sequence is given as a character vector of DNA sequences in the `.clone` argument.
#'
#' @param .clone A character vector of DNA sequences representing the TCR sequence to be analyzed
#' @param ... Additional arguments (currently not used)
#' @return A numeric value indicating the convergent recombination level of the TCR sequence
#' @export
cr_level <- function(clones, ignore.na=TRUE) {



}


# cr_level(xx, .cr_func = function(x) n_unique(x) / length(x))
cr_level <- function(.clone, .cr_func=n_unique, ...) {

  cr_sequences <- translate(.clone)

  indices <- vec_group_loc(cr_sequences)

  chopped <- vec_chop(.clone, indices$loc)

  vapply(chopped, .cr_func, numeric(1), ..., USE.NAMES = FALSE)

}


# cr_func(..., .func=n_unique)

# # An alternative implementation of `ave()` can be constructed using
# # `vec_chop()` and `list_unchop()` in combination with `vec_group_loc()`
# ave2 <- function(.x, .by, .f, ...) {
#   indices <- vec_group_loc(.by)$loc
#   chopped <- vec_chop(.x, indices)
#   out <- lapply(chopped, .f, ...)
#   list_unchop(out, indices = indices)
# }
#
# breaks <- warpbreaks$breaks
# wool <- warpbreaks$wool
#
# ave2(breaks, wool, mean)
# a <- translate(x)


get_cid <- function(.clone, ...) {

  dfl <- df_list(.clone, ..., .name_repair = "minimal")

  df <- new_data_frame(dfl)

  vec_group_id(df)

}

# TRUE for first nt of aa in a group (by ... or given cid)
is_cr_seq <- function(.clone, ..., .cid=NULL) {

  if (is.null(.cid))

    .cid <- get_cid(.clone, ...)


  vec_duplicate_any(translate(.clone)) & !duplicated(.cid)

}


cr_seq <- function()

cr_table <- function(.clone, .id) {

  dfl <- df_list(.clone, .id, .name_repair = "minimal")

  df <- new_data_frame(dfl)

  not_na <- vec_detect_complete(df)

  # not_duplicated <- !vec_duplicate_detect(df)

  # is_cr <- not_na & not_duplicated

  not_duplicated
  # df <- vec_slice(df, not_na)

  # df

  # df
  # cr_seq <- translate(.clone)
  #
  # l <- list(cr_seq, .id)
  #
  # apply(.clone, l, vec_unique_count)

}

cr_table(x,y)

vec_unique(data.frame(aa=a, id=y)) %>% vec_count(sort = "key")


share_table <- function(.clone, .id, ...) {





  # l <- vec_recycle_common(1, translate(.clone), .id)

  t

}
share_cr(x,y)



tapply(x, list(a, y), vec_unique_count)



cr2 <- function(.clone, .f=n_unique, ..., .cid=get_cid(.clone)) {
  indices <- vec_group_loc(.cid)$loc
  chopped <- vec_chop(.clone, indices)
  lapply(chopped, .f, ...)
  out <- lapply(chopped, .f, ...)
  list_unchop(out, indices = indices)
}

sharelvl <- function(.clone, .id, .f, ...) {
  cr2(.id, .clonotype = .clone)
}

cr2(xx, translate(xx))






cr_level_vec <- function(.clone, ..., .cr_func=cr_number) {

  l <- dots_splice(...)

  clone_list <- split(.clone, l)

  vapply(clone_list, .cr_func, numeric(1L), USE.NAMES = FALSE)

}

cr_level_tbl <- function(.clone, ..., .cr_func=cr_number) {

  dfl <- df_list(y,z,.name_repair = "minimal")

  tapply(.clone, dfl, .cr_func)

}

share_cr_levl <- function(.x, .y, .f1, f2, .freq=1) {



  l <- vec_recycle_common(.freq, .x, .y)

  tapply(l[[1]], l[-1], .f)

}

yy <- sample(LETTERS[1:10], 100, rep=T)

id <- get_cid(xx)

v1 <- cr2(xx, .cid = id)
v2 <- cr2(yy, .cid = id)


outer(unlist(v1), unlist(v2))

v1
v2
vec_chop(vec_cbind(v1, v2), vec_group_loc(id)$loc)








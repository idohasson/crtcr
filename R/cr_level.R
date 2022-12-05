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








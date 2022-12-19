# x <- rand_nt_vec()
l <- rand_nt_list()
# df <- rand_rep_df()
# tbl <- with(df, tapply(nt, list(clonotype=aa, repertoire=id, group=id2), cr_number))

cr_number(x)
cr_number <- function(nt) {

  if (rlang::is_list(nt))

    nt <- rlang::squash_chr(nt)

  vec_unique_count(nt)

}
# cr_prop(x)
# cr_prop(l)
cr_prop <- function(nt, val=cr_number(nt)) {

  val / vctrs::vec_size(nt)

}
# cr_subset(nt, 4:7)
# cr_subset(x, translate(x))
cr_subset <- function(nt, id=seq_along(nt), func=cr_prop) {

  nt_chopped <- cr_loc(nt, id)
  func(nt_chopped)

}
# vec_as_location(-1, 3, dimnames(tbl))
# cr_loc(c("E", "R", "K"), translate(x))
cr_loc <- function(nt, id) {

  i <- vec_group_loc(id)

  vec_chop(nt, i$loc)

}

cr_norm <- function(nt, index, id) {

  i <- vec_group_loc(translate(nt))

  x1 <- vec_chop(x, i$loc)

  x2 <- vec_chop(df$id, i$loc)

  mapply(cr_subset, x1, x2)

}

# vec_as_location(x, vec_size(x), x)

cr_cond <- function(nt, ..., condition=cr_prop(nt)>0) {
  # sum(as.logical(nt), na.rm = TRUE)>1
  condition_call <- substitute(condition)

  val <- list(nt, ...)

  eval(condition_call, val)

  # val <- do.call(func, l)
  # val
}
# vctrs::vec_as_location(i, n, aa)
# x <- rand_nt_vec()

condition <- function(..., .condition) {

  condition_call <- substitute(.condition)

  dfl <- df_list(..., .name_repair = "minimal")

  val <- new_data_frame(dfl)

  i_cond <- eval(condition_call, val)

  i_cond

}

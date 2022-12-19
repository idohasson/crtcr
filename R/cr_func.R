# sapply(rand_clone_list(), cr_prop)
cr_prop <- function(nt, ...) {

  vctrs::vec_unique_count(nt) / vctrs::vec_size(nt)

}

# with(rand_rep_df(), cr_subset(nt, aa))
cr_subset <- function(nt, i) {

  vctrs::vec_slice(nt, i)

}

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
cr_cond(nt=x, aa=translate(x), condition = cr_prop(nt)>.1)

cr_subset(x, sample(100, 10))

condition <- function(..., .condition) {

  condition_call <- substitute(.condition)

  dfl <- df_list(..., .name_repair = "minimal")

  val <- new_data_frame(dfl)

  i_cond <- eval(condition_call, val)

  i_cond

}

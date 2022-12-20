
# nt_list <- replicate(4, sample(S, sample(5,1), rep=T))
# group_cr_level(nt_list)
# group_cr_level(nt_list, group_func = mean)
# group_cr_level(nt_list, group_func = NULL)
# DF_R <- DF[DF$aa=="R", c("nt", "aa", "id")]
# grouped_nt <- split(DF_R$nt, DF_R$id)
# group_cr_level(grouped_nt, ~mean(level(.x, median)))
group_cr_level <- function(nt_list, group_level=mean) {

  if (is_formula(group_level))

    group_level <- as_function(group_level)

  if (is_function(group_level)) {

    levels <- cr_level(nt_list)

    group_level <- group_level(levels)

  } else if (is_null(group_level)) {

    group_nt <- unlist(nt_list, use.names = FALSE)

    group_level <- cr_level(group_nt)

  } else stop("group_func must be either a function, formula or NULL")

  return(group_level)
}

cr_level <- function(nt, aa=NULL, named=FALSE, group_level=mean) {
  cr_seq <- cr_list(nt, aa, named)
  levels <- vapply(cr_seq, vec_unique_count, integer(1))
  if (is_function(group_level)) {
    group_level(levels)
  } else if (is_null(group_level)) {
    return(levels)
  } else stop("group_level must be either a function or NULL")
}

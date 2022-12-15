library(rlang)
library(vctrs)
library(tibble)

# convergent_recombination_level <- function()
# NT=rand_nt_vec(100, 1);AA=translate(NT);ID=rep_along(NT, 1:4);FREQ=runif(100);DF=data.frame(nt=NT, aa=AA, id=ID, freq=FREQ)

# cr_number(NT[2:5],NT[5:7])
cr_number <- function(...) {

  nt_list <- dots_splice(...)

  vapply(nt_list, vec_unique_count, integer(1))
  # do.call(vec_unique_count, nt_list)

}

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


group_cr_level <- function(..., group_by) {

  dfl <- df_list(..., .name_repair = "unique_quiet")

  args <- new_data_frame(dfl)

  args

  # indices <- vec_group_loc(args)
  # grouped_nt <- vec_chop(.nt, indices$loc)
  # levels <- cr_level(grouped_nt)
  # levels

}

group_cr_level <- function(nt, ..., group_level=mean) {

  dfl <- df_list(..., .name_repair = "minimal")

  df <- new_data_frame(dfl)

  cr_df <- vec_split(nt, df)

  levels <- vapply(cr_df$val, cr_level, integer(1))

  group_level(levels)

}



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

cr_level_df <- function(nt, aa=NULL) {

  # vec_cbind(cr_df$key, CRlevel=level)
  cr_v <- clonorype_cr_level(nt, aa, named = TRUE)

  tbl <- tibble::enframe(cr_v, "clonotype", "CRlevel")

  as.data.frame(tbl)



}

# add_cr_level(DF, "nt", "aa")
add_cr_level <- function(df, nt_field=1, aa_field=NULL) {

  nt <- field(df, nt_field)

  aa <- if (is_null(aa_field)) {translate(nt)} else {field(df, aa_field)}

  cr_seq <- vec_group_loc(aa)

  cr_list <- vec_chop(nt, cr_seq$loc)

  each_row_level <- function(x) rep_along(x, vec_unique_count(x))

  cr_levels <- lapply(cr_list, each_row_level)

  levels_vec <- list_unchop(cr_levels, indices = cr_seq$loc)

  vec_cbind(df, CRlevel=levels_vec)

  # vec_cbind(clonotype=aa, clone=nt, CRlevel=levels_vec)

}

# cr_list(rand_nt_vec(100,1))
cr_list <- function(nt, aa=NULL, named=FALSE) {

  is_cr <- !duplicated(nt)

  nt <- nt[is_cr]

  if (is_null(aa)) {

    aa <- translate(nt)

  } else {

    aa <- aa[is_cr]

  }

  cr_split <- vec_split(nt, aa)

  if (isTRUE(named)) {

    return(tibble::deframe(cr_split))

  } else {

    return(cr_split$val)

  }

}

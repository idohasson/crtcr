library(vctrs)
library(magrittr)
library(rlang)
library(tibble)



cr_level(nt)
cr_level <- function(.clone) {

  apply_func(vec_count(.clone, sort = "location"), "count", sum)

}

# cr_level(nt, aa)
cr_level <- function(.clone, .cid) {

  apply_func(vec_split(.clone, .cid), "val", length)

}

# apply_func(vec_split(nt, aa), "val", length)
# apply_func(vec_count(nt, sort = "location"), "count", sum)


cr_func <- function (.clone, .freq=1, .each_cr=sum, .all_cr=length) {

  if (vec_size(.freq)==1) {

    .freq <- vec_recycle_common(.freq, .clone)

    .freq <- field(.freq, 1)

  } else {}



  .freq <- vec_split(.freq, .clone)



  freq_list <- field(.freq, "val")

  freq_list <- lapply(freq_list, .each_cr)

  freq_list <- unlist(freq_list)

  .all_cr(freq_list)

}

cr_level <- function (.clone, ...) {

  dfl <- df_list(..., .name_repair = "minimal")

  df <- new_data_frame(dfl)

  if (is_empty(df))

    return(.clone)

  grouped_df <- vec_split(.clone, df)


  clone_list <- field(grouped_df, "val")

  lapply(clone_list, vec_unique_count)

  # clone_list


}


unique_count <- function(x, .unique_freq=length) {}

freq_func <- function(.values, .var, .func) {

  value_list <- vec_split(.values, .var)

  l <- field(value_list, "val")

  func_val <- lapply(l, .func)

  if (FALSE) return(func_val)

  field(value_list, "val") <- func_val

  deframe(value_list)

}

x <- rand_nt_vec()
.clonotype <- translate(x)
clone <- vec_split(x, .clonotype)
freq_func(clone, var="val", .func=length)
freq_func(clone, var="val", .func=vec_unique_count)
deframe(clone)

xx <- vec_count(x, sort = "location")
xx <- vec_count(translate(x), sort = "location")
freq_func(xx, "count", length)
freq_func(xx, "count", sum)



# unique_df(.x=1:3,.y=3:1)
unique_df <- function(...) {

  dfl <- df_list(..., .name_repair = "unique_quiet")

  df <- new_data_frame(dfl)

  vec_unique(df)

}

get_unique <- function(... , .of_col=1, .func=unique) {

  dfl <- df_list(..., .name_repair = "minimal")

  df <- new_data_frame(dfl)

  if (is_character(.of_col))

    .of_col <- which(fields(df) %in% .of_col)

  if (length(.of_col) != 1)

    stop("get_unique")

  by_values <- field(df, .of_col)

  df_split <- vec_split(df[,-.of_col], by_values)



  field(df_split, "val") <- lapply(df_split$val, .func)



  # fields(df)

  # df_split <- vec_split(df[,-.fields], df[,.fields])
  #
  # results <- lapply(df_split, .func, ...)
  #
  # df_split
  # # .df[,.fields]
  # vec_group_loc(for_each_unique)

}



each_unique <- function(..., .by_each=1, .each_func=length,
                        name_vec=FALSE, flat_vec=FALSE) {

  dfl <- df_list(..., .name_repair = "unique")

  df <- new_data_frame(dfl)

  for_each_unique <- field(df, .by_each)

  apply_indices <- vec_group_loc(for_each_unique)

  chopped_df <- vec_chop(df, apply_indices)

  each_results <- lapply(chopped_df, .each_func, ...)

  each_results
  # loc_list <- deframe(apply_indices)

  # unlist(each_results, recursive = flat_vec, use.names = name_vec)

}



count_id <- function(x, .split_func=length, ...) {

  split_x <- vec_split(x[,1], x[,-1])

  unique_x <- sapply(split_x$val, vec_unique_count)

  .split_func(unique_x, ...)

}


each_clonotype <- function(dfl, ff, ...) {

  uf <- function(x, i, f, ...) {

    s <- vec_split(x, field(x, i))

    field(s,"val") <- lapply(s$val, f, ...)

    s

  }


  r <- lapply(dfl, uf, i=".gid", f=ff, ...)

  enframe(r)

}


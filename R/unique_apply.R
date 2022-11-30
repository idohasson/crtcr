

# across_unique <- function(.data, .var, apply_func, ...) {
#
#   # dfl <- df_list(..., .name_repair = "minimal")
#
#   split_dfl <- vec_split(.data, field(.data, .var))
#
#   result_list <- lapply(split_dfl$val, apply_func, ..., USE.NAMES = FALSE)
#
#   vec_cbind(split_dfl$key, result_list)
#
#
#
#
#   # each_apply <- tapply(.x, vec_group_id(dfl), apply_func)
#
#
#   # if (!is.null(.aggr_func))
#   #
#   #   return(.aggr_func(each_apply))
#
#
#
#   # df <- vec_cbind(vec_unique(dfl),
#   #                 as.vector(each_apply),
#   #                 .name_repair = "minimal")
#   #
#   # setNames(df, c(names(dfl), substitute(apply_func)))
#
# }

across_unique <- function(.over, .by, apply_func, ...) {

  # f <- as.factor(vec_group_id(.by))

  split_dfl <- split(.over, vec_group_id(.by))
  split_dfl
  # vapply(split_dfl, apply_func, integer(1L), USE.NAMES = F)

  # dfl <- new_data_frame(as.list(.by))
  #
  # split_dfl <- split(.over, dfl, drop = F)
  #
  # return(split_dfl)
  # vec_split(.by_dfl, .of_dfl)


  # split_dfl <- vec_split(.by_dfl, .of_dfl)
  # split_dfl <- vec_split (
  #
  #   .data[fields(.data) != .across_var],
  #
  #   # .data[ - which.min(fields(.data) == .across_var) ],
  #
  #   field(.data, .across_var)
  #
  # )

  # split_dfl$val

  # lapply(split_dfl$val, apply_func, ..., USE.NAMES = FALSE)

  # results <- sapply(split_dfl$val, apply_func, ..., USE.NAMES = FALSE)
  #
  # vec_cbind(split_dfl$key, results, .name_repair = "unique")

}


apply_each <- function(.data, .to_var, .by_vars, .with_func, ...) {

  tapply(

    field(.data, .to_var),

    # .data[[.to_var]],
    .data[.by_vars],

    .with_func, ...
  )

}

aggregate_at <- function(.data, .margin, .aggr_func, ...) {

  apply(.data,

        .margin,

        .aggr_func, ...,simplify = FALSE)

}


# across_unique(.data = x,
#               .across_var = "aa",
#               apply_func = apply_each,
#               .to_var="group_id",
#               .by_vars=c("nt", "sample_id"),
#               .with_func=unique)
#

# x3 <- tapply(field(x, "nt"), x[c("aa", "group_id", "sample_id")], c)

# apply(x3, c("aa", "sample_id"), cr_level)
# apply(x3, c("aa", "group_id"), cr_level)
# apply(x3, c("aa"), cr_level)



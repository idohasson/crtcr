condition_unique <- function(..., .condition) {

  condition_call <- substitute(.condition)

  dfl <- df_list(..., .name_repair = "minimal")

  df <- new_data_frame(dfl)

  df_unique <- vec_unique(df)

  i_cond <- eval(condition_call, df_unique)

  df_unique[i_cond, ]
}

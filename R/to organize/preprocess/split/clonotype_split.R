clonotype_split <- function(aa, id, ...) {

  # id_list <- dots_splice(id, ...)
  dfl <- df_list(id, ..., .name_repair = "unique_quiet")

  df <- new_data_frame(dfl, class = "clonotype")

  clonotype_df <- vec_split(df, aa)

  tibble::deframe(clonotype_df)

}

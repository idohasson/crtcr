group_func <- function(group_var) {

  function (...) {

    vars <- dots_splice(...)

    dfl <- df_list(..., .name_repair = "minimal")

    data <- new_data_frame(dfl)

    if (!has_name(data, group_var))

      group_var <- which.max(!have_name(data))

    by <- field(data, group_var)

    clonotype_df <- vec_split(data, by)

    tibble::deframe(clonotype_df)

  }

}

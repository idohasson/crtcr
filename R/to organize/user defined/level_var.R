
level_var <- function(data, level_func, group_func) {

  data_fragments <- group_func(data)

  vapply(data_fragments, do.call, numeric(1), what=level_func)

}

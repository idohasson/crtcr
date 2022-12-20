# prepare_list(replicate(2, replicate(3, rand_nt_list(v = sample(4, 1)), F), F))
prepare_list <- function(data_list) {

  flat_list <- squash_if(data_list, is_list)

  data <- map_dfr(flat_list, as_tibble_col, column_name = "clone", .id = "id")

  prepare_df(data, .nt = clone, id)

}

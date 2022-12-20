merge_datasets <- function(..., by = "row") {
  # Merge multiple datasets containing TCR clonotype sequences
  datasets <- lapply(..., as.data.frame)
  # dfl <- lapply(..., as.data.frame)
  if (by == "row") {
    sequences <- dplyr::bind_rows(datasets, .id = ".rid")
    # sequences <- do.call(rbind, datasets)
  } else if (by == "column") {
    sequences <- do.call(cbind, datasets)
  }
  return(sequences)
}
# TODO: Use `bind_data_frames` inside of `merge_datasets()`
# Define the repertoire_group_data_frame() function
# bind_data_frames <- function(df, ..., id_name="id") {
#   # Bind the rows of the input data frames together
# have_name()
#   # dplyr::bind_rows(df, ..., .id = id_name)
#   # vctrs::vec_rbind(..., .name_repair = "unique_quiet")
# }

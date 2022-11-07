# df_lists <- replicate(4, rand_group(), FALSE)
# rand_df <- group_join(df_lists)
# names(rand_df)
# share_table(rand_df, "clonotype", "group", "rep_id")
share_table <- function(df, clonotype, group_id, rep_id) {

  unique_by <- dplyr::vars(c(clonotype, group_id, rep_id))

  dplyr::distinct_at(df, dplyr::all_of(unique_by)) %>%

    dplyr::select(clonotype, group_id) %>% table
}


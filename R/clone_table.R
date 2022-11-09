# # df_lists <- replicate(4, rand_group(), FALSE)
# # group_df <- group_join(df_lists)
# # head(group_df)
# # clone_table(group_df, clonotype = "clonotype", by_col="group")
# clone_table <- function(df, clonotype, by_col, ...) {
#
#   unique_by <- dplyr::vars(c(clonotype, by_col, ...))
#
#   dplyr::distinct_at(df, all_of(unique_by), .keep_all = TRUE) %>%
#
#     dplyr::select(clonotype, by_col) %>% table
#
# }

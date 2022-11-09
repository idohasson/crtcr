#'
#'
#' #' CR level table
#' #'
#' #' @param ... repertoire list / data frames vectors
#' #'
#' #' @return table
#' #'
#' #' @importFrom dplyr distinct_at n_distinct
#' #' @importFrom tidyr unite pivot_wider
#' #' @importFrom tibble column_to_rownames
#' #' @importFrom magrittr %>%
#' #'
#' #' @export
#' #'
#' #' @examples
#' #'
#' #' # generate random
#' #' df_lists <- replicate(4, rand_group(), FALSE)
#' #' rand_df <- group_join(df_lists)
#' #' names(rand_df)
#' #' unite(col = "id", c("rep_id" ,"group"), sep = "/")
#'
#'
#' # cr_level(df = rand_df, clone = "clone", clonotype = "clonotype", rep_id = "rep_id" ,"group")
#'
#'
#' #' id_df <- unite(rand_df, col = "id", c("group", "rep_id"), sep = "/")
#' #' cr_level(df = id_df, clone = "clone", clonotype = "clonotype", rep_id = "id")
#' #'
#' cr_level <- function(df, clone, clonotype, rep_id, ...) { # TODO: allow data frame input of 'group_join' output
#'
#'
#'   clone_table(id_df, clonotype = clonotype, by_col = id, clone, ...)
#'
#'   # unique_clone_df <- distinct_at(df, all_of(vars(c("group", "rep_id", "clone"))), .keep_all = TRUE)
#'   #
#'   # cr_level <- unite(unique_clone_df, col = "id", c("group", "rep_id"), sep = "/") %>%
#'   #
#'   #   select_at(all_of(vars(c("clonotype", "id")))) %>% table
#'   #
#'   # cr_level
#'
#'   # unite(df, group, rep_id, col = "id") %>%
#'   # distinct_at(c("id", "clone"), .keep_all=TRUE) %>%
#'   # pivot_wider(names_from = id,
#'   #                    values_from = clone,
#'   #                    values_fn = n_distinct) %>%
#'   # column_to_rownames("clonotype") %>% as.matrix()
#'
#' }
#'
#'
#'
#' # clone_table(group_df, clonotype = "clonotype", by_col="group", "rep_id")
#' # df_lists <- replicate(4, rand_group(), FALSE)
#' # group_df <- group_join(df_lists)
#' #
#' # clonotype_share_level <- unique_clontype_df %>%
#' #
#' #   select_at(all_of(vars(c("clonotype", "group")))) %>%
#' #
#' #   table
#' # # head(group_df)
#' # clone_table(group_df, clonotype = "clonotype", by_col="group", "rep_id")
#' # clone_table(group_df, clonotype = "clonotype", by_col="group", "rep_id")
#'
#'
#'
#'
#'

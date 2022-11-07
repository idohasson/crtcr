

#' CR level table
#'
#' @param ... repertoire list / data frames vectors
#'
#' @return table
#'
#' @importFrom dplyr distinct_at n_distinct
#' @importFrom tidyr unite pivot_wider
#' @importFrom tibble column_to_rownames
#' @importFrom magrittr %>%
#'
#' @export
#'
#' @examples
#'
#' # generate random
#' g <- replicate(4, rand_group(), simplify = FALSE)
#'
#' cr_level(g)
#'
cr_level <- function(...) { # TODO: allow data frame input of 'group_join' output

  df <- group_join(...)


  unique_clone_df <- distinct_at(df, all_of(vars(c("group", "rep_id", "clone"))), .keep_all = TRUE)

  cr_level <- unite(unique_clone_df, col = "id", c("group", "rep_id"), sep = "/") %>%

    select_at(all_of(vars(c("clonotype", "id")))) %>% table

  cr_level

  # unite(df, group, rep_id, col = "id") %>%
  # distinct_at(c("id", "clone"), .keep_all=TRUE) %>%
  # pivot_wider(names_from = id,
  #                    values_from = clone,
  #                    values_fn = n_distinct) %>%
  # column_to_rownames("clonotype") %>% as.matrix()

}






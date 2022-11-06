

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

  unite(df, group, rep_id, col = "id") %>%

  distinct_at(c("id", "clone"), .keep_all=TRUE) %>%

  pivot_wider(names_from = id,
                     values_from = clone,
                     values_fn = n_distinct) %>%

  column_to_rownames("clonotype") %>% as.matrix()

  # df %<>% distinct(group, rep_id, clone, .keep_all = TRUE)
  # df %$% table(clonotype, rep_id, group) %>%
  # ftable(row.vars="clonotype", col.vars="rep_id") %>% as.table()

}



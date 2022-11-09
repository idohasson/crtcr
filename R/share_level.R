#' Count table for number of each clonotype found in a repertoire in a specific gorup
#'
#' @param df data frame with clonotype column, group ID coliumn and repertoire ID column
#' @param clonotype clonotype column name
#' @param group_id Group ID column name
#' @param rep_id Repertoire ID column name
#' @param ... any additional column name
#'
#' @return table
#'
#' @export
#'
#' @examples
#'
#' df_lists <- replicate(4, rand_group(), FALSE)
#' rand_df <- group_join(df_lists)
#' share_table(rand_df, "clonotype", "group", "rep_id")
#'
share_table <- function(df, clonotype, group_id, rep_id, ...) {

  unique_by <- c(clonotype, group_id, rep_id, ...)

  df[unique_by] %>% distinct() %>%

    select(unique_by[1:2]) %>% table
}


# gl <- replicate(3, rand_group())
share.table <- function(group_list) {
  # library(purrr)
  # library(dplyr)
  # library(tidyr)
  # library(tibble)
  # library(vctrs)

  stopifnot(every(group_list, is.list))
  stopifnot(vec_depth(group_list) == 3)
  list(list(1:5), list(1:4), list(1:6)) %>%
    set_names(LETTERS[seq_along(.)]) %>%
    as_tibble_col()
  # if (clonal_seq)
  group_list <- modify_depth(group_list, 2, translate)

  map_depth(group_list, 2, unique) %>%
  # map(flatten_chr) %>%
  # counts the number of times each clonotype appears in a sample vector
  # map_dfr(. %>% flatten_chr %>% table, .id = "group")
  map(. %>% flatten_chr)
  # takes the group column and the count column and spreads the count
  # column into multiple columns, one for each unique value in the group
  # column. The resulting data frame has one row for each unique value
  # in the group column and one column for each unique value in the
  # count column.

  # spread(group, count, 0) %>%  # convert the resulting data frame
  # # from long format to wide forma
  # column_to_rownames("key")   # convert the group column into row names
}


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


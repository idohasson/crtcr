
#' Title
#'
#' @param df
#' @param aa_col
#' @param match_var
#'
#' @return
#'
#' @export
#'
#' @examples
cr_level <- function(df, aa_col, match_var) {

  clonotype_df <- df %>%

    distinct_at(union(aa_col, match_var), .keep_all = TRUE)

  pull(clonotype_df, aa_col) %>%

    vec_count() %>%

    rename(clonotype = "key", cr_level = "count")
}





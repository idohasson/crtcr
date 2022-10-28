

#' Title
#'
#' @param df
#' @param aa_col
#' @param match_var
#'
#' @return
#' @export
#'
#' @examples
top_cr <- function(df, aa_col, match_var, n=1) {

  cr_level(df, aa_col, match_var) %>%

    head(n) %>%

    left_join(df, by = c(clonotype = aa_col), keep = TRUE)

}

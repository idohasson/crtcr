
#' Share-level of the group's repertoires
#'
#' @param clonotype clonotype sequences vector
#' @param rep_id repertoire ID vector
#'
#' @return share-level vector named by clonotype sequence
#' @export
#'
#' @examples
#'
#' sample_id <- rep(c("A", "B"), each=3)
#'
#' clonotype_vec <- c("M", "F", "I", "F", "L", "L")
#'
#' share_level(sample_id, clonotype_vec)
#'
share_level <- function(rep_id, clonotype) {

  results <- tapply(rep_id, clonotype, n_distinct)

  if (length(results)==1)
    return(unname(results))
  else
    return(results)
}


#' Compute the share level of clonotypes in a group
#'
#' @param data data frame
#' @param clonotype_var clonotype sequence column name
#' @param rid repertoire identification column name
#' @param ... group by
#'
#' @return data frame
#' @export
#'
#' @examples
#'
#' require(dplyr)
#'
#' df <- data.frame(nt=c("ATG","TTC","TAT","TTT","ATG","ATG"),
#'                  aa=c("M","F","Y","F","M","M"),
#'                  rep_id = gl(3,1, 6),
#'                  group_id = gl(2, 3, labels = c("cancer", "control")))
#' df %>%
#' group_by(group_id) %>%
#' share_level_df(aa, rep_id)
#'
#' # Same as:
#' share_level_df(df, aa, rep_id, group_id)
#'
share_level_df <- function(data, clonotype_var, rid, ...) {

  group_by(data, {{clonotype_var}}, ..., .add = FALSE) %>%

  summarise(across({{rid}}, n_distinct, .names = "share"), .groups = "drop")

}

#' Title x
#'
#' @param df x
#' @param clonotype_var x
#' @param rid x
#' @param ... x
#'
#' @return x
#' @export
#'
#' @examples
#'
#' df <- data.frame(nt=c("ATG","TTC","TAT","TTT","ATG","ATG"),
#'                  aa=c("M","F","Y","F","M","M"),
#'                  rep_id = gl(3,1, 6),
#'                  group_id = gl(2, 3, labels = c("cancer", "control")))
#'
#'  share_level_table(df, aa, rep_id, group_id)
#'
share_level_table <- function(df, clonotype_var, rid, ...) {

  df %>%

    share_level_df({{clonotype_var}}, {{rid}}, ...) %>%

    xtabs(formula = share ~ .)

}

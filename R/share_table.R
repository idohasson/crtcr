#' Computes sharing level of unique clones among samples
#'
#' @description Counts the number of times each clonotype appears in a sample vector
#'
#' @param clonotype_list list of character vectors
#'
#' @return
#' @export
#'
#' @examples
#'
#' rand_rep <- function(n, alphabet = LETTERS[1:10])
#'    sample(alphabet, n, replace = TRUE)
#'
#' rep_list <- replicate(3, rand_rep(rpois(1, 10)), simplify = FALSE)
#'
#' share_table(rep_list)
#'
share_table <- function(clonotype_list) {

  clonotype_list %>%
    # counts the number of times each clonotype appears in a sample vector
    purrr::map_dfr(vec_count, .id = "group") %>%
    # takes the group column and the count column and spreads the count
    # column into multiple columns, one for each unique value in the group
    # column. The resulting data frame has one row for each unique value
    # in the group column and one column for each unique value in the
    # count column.
    tidyr::spread(group, count, 0) %>%  # convert the resulting data frame
                                        # from long format to wide forma
    tibble::column_to_rownames("key")   # convert the group column into
                                        # row names
}

# rand_rep <- function(n, alphabet = LETTERS[1:10])
#   sample(alphabet, n, replace = TRUE)
#
# rep_list <- replicate(3, rand_rep(rpois(1, 10)), simplify = FALSE)
#
# share_table(rep_list) %>%



# share_table(list(c1, c2, c3)) %>%

#   as.matrix() %>%
#
#   prop.table(margin = 2) %>%
#
#   t %>%
#
#   as.data.frame %>%
#
#   cbind(type = cr_type3(.)) %>%
#
#   reshape2::melt(id.vars = "type",
#        variable.name = "group",
#        value.name = "share") %>%
#
#   xtabs(formula = share ~ type + group)
#
# xtabs(formula = share ~ type + group) / nrow(df)



# TODO: make generic

#' calculate the convergent recombination level (CR level) of a sample's clonotypes
#'
#' @description Determine the convergent recombination level (CR level) by counting the number of times each clonotype occurs.
#'
#' @param clonotype character vectors
#' @param match_clones_with same size as 'clonotype' character vectors
#'
#' @return
#'
#' @export
#'
#' @examples
#'
#' # From the letter vector representing the clonotype sequences,
#' # generate a random list of letters.
#' clonotype_vec <- sample(LETTERS[1:10], 20, replace = TRUE)
#'
#' clone_vec <- sample(LETTERS[1:2], 20, replace = TRUE)
#'
#'
cr_level <- function(clonotype, match_clones_with) {
  # checking whether the match_clones_with argument is missing.
  # If it is, then the function will simply return the clonotype vector.
  if (!missing(match_clones_with)) {
    # checking same size vector
    stopifnot(length(clonotype) == length(match_clones_with))
    # bind the clonotype vector and the match_clones_with vector together.
    clonotype <- cbind.data.frame(match_clones_with, clonotype) %>%
      # remove any duplicate rows from the data frame.
      unique() %>% pull()
      # and pull out the clonotype column from the data frame.
    }
  # count the number of times each clonotype occurs.
  vec_count(clonotype) %>%
    # return the clonotype column from the data frame
    rename(clonotype = "key", cr_level = "count")
    # with the corresponding CR level.
}


# cr_level.data.frame <- function(df, aa_col, match_var) {
#
#   clonotype_df <- df %>%
#
#     distinct_at(union(aa_col, match_var), .keep_all = TRUE)
#
#   pull(clonotype_df, aa_col) %>%
#
#     vec_count() %>%
#
#     rename(clonotype = "key", cr_level = "count")
# }


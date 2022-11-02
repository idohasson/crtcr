# cr_level



  # cr_level_clonotype

# cr_level_clonotype(df, c("S", "R", "T"), "aa", "nt")
cr_level_clonotype <- function(rep, seq, clonotype, clone) {

  rep[rep[,clonotype] %in% seq, c(clonotype, clone)] %>% distinct() %>%

    pull(clonotype) %>% cr_level_rep()
}

# cr_level_rep

cr_level_rep <- function(rep, seq, clonotype, clone) {
# cr_level_rep <- function(clonotype, match_clones_with) {
  # checking whether the match_clones_with argument is missing.
  # If it is, then the function will simply return the clonotype vector.


  if (!missing(clonotype))
    clone_df <- distinct(rep[c(clonotype, clone)])

  # count the number of times each clonotype occurs.
  vec_count(clonotype) %>%
    # return the clonotype column from the data frame
    rename(clonotype = "key", cr_level = "count")
  # with the corresponding CR level.
}

# cr_level_group

cr_level_group <- function(clonotype_list) {
  # Checking if the input list is named
  if (is.null(names(clonotype_list))) {
    # Assign the sequence of numbers to the names of the clonotype_list.
    names(clonotype_list) <- seq_along(clonotype_list)
  }
  # counts the number of times each clonotype appears in each sample.
  df <-  purrr::map_dfr(clonotype_list, vec_count, .id = "sample") %>% rename(clonotype = "key")
  # sums the counts for each clonotype and sample vector.
  tapply(df$count, df[c("clonotype", "sample")], sum, default = 0)
}

# cr_level_compare

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
# cr_level <- function(clonotype, match_clones_with) {
#   # checking whether the match_clones_with argument is missing.
#   # If it is, then the function will simply return the clonotype vector.
#   if (!missing(match_clones_with)) {
#     # checking same size vector
#     stopifnot(length(clonotype) == length(match_clones_with))
#     # bind the clonotype vector and the match_clones_with vector together.
#     clonotype <- cbind.data.frame(match_clones_with, clonotype) %>%
#       # remove any duplicate rows from the data frame.
#       unique() %>% pull()
#     # and pull out the clonotype column from the data frame.
#   }
#   # count the number of times each clonotype occurs.
#   vec_count(clonotype) %>%
#     # return the clonotype column from the data frame
#     rename(clonotype = "key", cr_level = "count")
#   # with the corresponding CR level.
# }


#' Make a table that specifies the CR values for each individual's clonotypes.
#'
#' @description claculate the CR level in each clonotype in every sample.
#' The number of various NT sequences encoding a particular AA sequence
#' is defined as the CR level in this context.
#'
#' @param clonotype_list
#'
#' @return return a matrix with the cr-level of the individual in the column
#' in each row of the clonotype
#'
#' @export
#'
#' @examples
#'
#' # From the letter vector representing the clonotype sequences,
#' # generate a random list of letters.
#' l <- replicate(6, sample(LETTERS, sample(13:18, 1, replace = TRUE)), simplify = FALSE)
#'
#' cr_level_matrix(l)
#'
# cr_level_matrix <- function(clonotype_list) {
#   # Checking if the input list is named
#   if (is.null(names(clonotype_list))) {
#     # Assign the sequence of numbers to the names of the clonotype_list.
#     names(clonotype_list) <- seq_along(clonotype_list)
#   }
#   # counts the number of times each clonotype appears in each sample.
#   df <-  purrr::map_dfr(clonotype_list, vec_count, .id = "sample") %>% rename(clonotype = "key")
#   # sums the counts for each clonotype and sample vector.
#   tapply(df$count, df[c("clonotype", "sample")], sum, default = 0)
# }



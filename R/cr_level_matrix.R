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
cr_level_matrix <- function(clonotype_list) {
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

cr_table <- function(df_list, ...) {

  get_clonotypes_grouped(df_list, indices=i1, nt_col="nt", aa_col="aa")

  with(clono_g1, table(clonotype, sample))
}

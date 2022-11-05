#' CR-level of a selected clonotypes
#'
#' @param rep df
#' @param of sequences
#' @param clonotype_col name
#' @param clone_col name
#'
#' @return
#' @export
#'
#' @examples
#'
#' rand_rep_df(seq_n=1000, seq_l=3) %>%
#'
#'   cr_level_clonotype(c("S", "T", "M"), "aa","nt")
#'
cr_level_clonotype <- function(rep,  of, clonotype_col, clone_col) {

  filter(rep ,if_all(all_of(clonotype_col), ~ .x %in% of)) %>%

    group_by_at(clonotype_col) %>%

    summarise_at(clone_col, n_distinct) %>%

    setNames(c("clonotype", "CRlevel"))
}

#' calculate the convergent recombination level (CR level) of a sample's clonotypes
#'
#' @description Determine the convergent recombination level (CR level) by counting the number of times each clonotype occurs.
#'
#' @param clonotype character vectors
#' @param match_clones_with same size as 'clonotype' character vectors
#'
#' @return df
#'
#' @export
#'
#' @examples
#'
#' # From the letter vector representing the clonotype sequences,
#' # generate a random list of letters.
#'
#' nt <- rand_rep_vec()
#' aa <- translate(nt)
#'
#' cr_level(aa, nt)
#'
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
    clonotype <- cbind.data.frame(match_clones_with, clonotype)
      # remove any duplicate rows from the data frame.
    clonotype <- pull(distinct(clonotype))
    # and pull out the clonotype column from the data frame.
  }
  # count the number of times each clonotype occurs.
  vec_count(clonotype, sort = "none") %>%
    # return the clonotype column from the data frame with the
    setNames(c("clonotype", "CRlevel"))# corresponding CR level.
}


#' CR level of vector list
#'
#' @param rep list
#'
#' @return data frame
#' @export
#'
#' @examples
cr_level_rep <- function(rep) {

}



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
#' @importFrom purrr map_dfr
#' @importFrom dplyr rename
#'
#' @export
#'
#' @examples
#' # From the letter vector representing the clonotype sequences,
#' # generate a random list of letters.
#'
#' rep_list <- rand_group(seq_n = 3)
#'
#' cr_level_group(rep_list, "aa", "nt")
#'
#' dfl <- rand_group(seq_n = 3) %>% lapply(function(df) df[,2:1]) %>%
#'
#'    lapply(setNames, names(formals(cr_level)))
#'
#'
#'
cr_level_group <- function(clonotype_list, clonotype_col, clone_col) {
  # Checking if the input list is named
  if (is.null(names(clonotype_list))) {
    # Assign the sequence of numbers to the names of the clonotype_list.
    names(clonotype_list) <- seq_along(clonotype_list)
  }
  clonotype_list <- clonotype_list %>%
    map(distinct_at, clone_col, .keep_all = TRUE) %>%
    map(pull, clonotype_col)
    # counts the number of times each clonotype appears in each sample.
  df <-  map_dfr(clonotype_list, vec_count, .id = "sample") %>%
    rename(clonotype = "key")
  # sums the counts for each clonotype and sample vector.
  tapply(df$count, df[c("clonotype", "sample")], sum, default = 0)
}







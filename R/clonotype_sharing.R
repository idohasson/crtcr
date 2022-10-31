# clonotype_sharing



# shared_clonotype
# df_list to numerical vector
#' count clonotype-sharing among individuals
#'
#' @description for a given list of amino acid sequences vectors of each individual/sample count number of samples each clonotype is presented
#'
#' @param clonotype_list list of character vectors
#'
#' @return data frame - number of vectors each sequence presented in
#'
#' @export
#'
#' @examples
#' # Generate a random list of letters (as the clonotype sequences) from the letters vector.
#' l <- replicate(6, sample(LETTERS, sample(13:18, 1, replace = TRUE)), simplify = FALSE)
#' share_level(l) # number of vectors a letter found in
#'
shared_clonotype <- function(rep_list, clonotype_col) {
  # make sure each vector has unique sequences
  clonotype_list <- get_clonotypes_list(rep_list, aa_col=clonotype_col, out_type="unique")
  # flatten the list of lists into a single list
  clonotype <- unlist(clonotype_list)
  # generate a frequency table of the clonotype sample collection
  clonotype_n <- table(clonotype)
  # convert the frequency table into a data frame
  as.data.frame(clonotype_n, responseName = c("sharing"))
}

rand_group(rep_type="df_list", n_sample=10, seq_n = 100, seq_len=5) %>%

  share_level(clonotype_col = "aa")


# shared_clone

# shared_population_clonotype

# shared_population_clone

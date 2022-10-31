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
#'
#' rand_group(rep_type="df_list", n_sample=10, seq_n = 100, seq_len=5) %>%
#'
#'   shared_clonotype(clonotype_col = "aa")
#'
shared_clonotype <- function(rep_list, clonotype_col) {
  # make sure each vector has unique sequences
  clonotype_list <- get_clonotypes_list(rep_list, aa_col=clonotype_col, out_type="unique")
  # flatten the list of lists into a single list
  clonotype <- unlist(clonotype_list)
  # generate a frequency table of the clonotype sample collection
  clonotype_n <- vec_count(clonotype)

  setNames(clonotype_n, c("clonotype", "shared"))

  # convert the frequency table into a data frame
  # as.data.frame(clonotype_n, responseName = c("sharing"))
}


shared_clonotype <- function(rep_list, clonotype_col) {
  # make sure each vector has unique sequences
  clonotype_list <- get_clonotypes_list(rep_list, aa_col=clonotype_col, out_type="unique")
  # flatten the list of lists into a single list
  clonotype <- unlist(clonotype_list)
  # generate a frequency table of the clonotype sample collection
  clonotype_n <- vec_count(clonotype)

  setNames(clonotype_n, c("clonotype", "shared"))

  # convert the frequency table into a data frame
  # as.data.frame(clonotype_n, responseName = c("sharing"))
}


rand_group(rep_type="df_list", n_sample=10, seq_n = 100, seq_len=5) %>%
  # get_clonotypes_list(nt_col="nt",aa_col="aa") %>% unlist() %>% vec_count() %>% head
  shared_clonotype(clonotype_col = "aa")

rep_list <- rand_group(rep_type="df_list", n_sample=10, seq_n = 100, seq_len=5)
i1 <- 1:6; i2 <- 7:10

p <- lapply(list(i1, i2), function(i) get_clonotypes_list(rep_list[i], nt_col="nt", aa_col="aa", to_df = TRUE))

lapply(p, distinct_at, .vars = c("clonotype", "sample")) %>%

  lapply(pull)






lapply(function(df) head(with(df, table(clonotype, sample))))

rand_group(rep_type="df_list", n_sample=10, seq_n = 100, seq_len=5) %>%

  share_level(clonotype_col = "aa")


# shared_clone

# shared_population_clonotype

# shared_population_clone

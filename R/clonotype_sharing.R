
# shared_clonotype - number of samples with clonotype
# shared_clonotype
# rand_group(rep_type="df_list", n_sample=10, seq_n = 100, seq_len=5) %>%
#   shared_clonotype(clonotype_col = "aa")
shared_clonotype <- function(rep_list, clonotype_col) {

  if (every(rep_list, is.data.frame))

    rep_list <- lapply(rep_list, get_aa, clonotype_col)

  share_level(rep_list)
  # clonotype_list <- lapply(rep_list, get_aa, clonotype_col) %>% lapply(unique)
  # vec_count(unlist(clonotype_list)) %>%
  #   setNames(c("clonotype", "shared"))
}


# shared_clonotype <- function(rep_list, clonotype_col) {
#   # make sure each vector has unique sequences
#   clonotype_list <- get_clonotypes_list(rep_list, aa_col=clonotype_col, out_type="unique")
#   # flatten the list of lists into a single list
#   clonotype <- unlist(clonotype_list)
#   # generate a frequency table of the clonotype sample collection
#   clonotype_n <- vec_count(clonotype)
#
#   setNames(clonotype_n, c("clonotype", "shared"))
#
#   # convert the frequency table into a data frame
#   # as.data.frame(clonotype_n, responseName = c("sharing"))
# }

# shared_clone
# rand_group(rep_type="df_list", n_sample=10, seq_n = 100, seq_len=5) %>%
#   shared_clone(clone_col = "nt")
shared_clone <- function(rep_list, clone_col) {

  if (every(rep_list, is.data.frame))

    rep_list <- lapply(rep_list, get_nt, clone_col)

  share_level(rep_list)
  # rep_list %>%
  #   lapply(unique) %>%
  #   unlist()
  #   vec_count() %>%
  #   setNames(c("clone", "shared"))
}


# shared_population_clonotype(rep_list, "aa", list(1:5, 6:10))
shared_population_clonotype <- function(rep_list, clonotype_col, population_i) {

  stopifnot(every(rep_list, is.data.frame))

  get_clonotype_aa <- function(indices)
    lapply(rep_list[indices], get_aa, clonotype_col)


  rep_list <- lapply(population_i, get_clonotype_aa)

  share_table(rep_list)

  # lapply(rep_list[indices], get_aa, clonotype_col)
  # share_list <- lapply(populations_list, shared_clonotype, clonotype_col=clonotype_col)
  # purrr::reduce(share_list, full_join, by="clonotype")
}

# shared_population_clone
# rand_group(rep_type="df_list", n_sample=10, seq_n = 100, seq_len=5) %>%
#   shared_population_clone("nt", list(1:5, 6:10))
shared_population_clone <- function(rep_list, clone_col, population_i) {

  stopifnot(every(rep_list, is.data.frame))

  get_clone_nt <- function(indices)
    lapply(rep_list[indices], get_nt, clone_col)

  rep_list <- lapply(population_i, get_clone_nt)

  share_table(rep_list)

  # populations_list <- lapply(populations, function(indices) rep_list[indices])
  # share_list <- lapply(populations_list, shared_clone, clone_col=clone_col)
  # purrr::reduce(share_list, full_join, by="clone")
}
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
share_level <- function(clonotype_list) {
  # make sure each vector has unique sequences
  clonotype_list <- lapply(clonotype_list, unique)
  # flatten the list of lists into a single list
  clonotype <- unlist(clonotype_list)
  # generate a frequency table of the clonotype sample collection
  clonotype_n <- table(clonotype)
  # convert the frequency table into a data frame
  as.data.frame(clonotype_n, responseName = c("shared"))
}

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
share_table <- function(populations_list) {

  stopifnot(every(populations_list, is.list))

  if (vec_depth(populations_list) == 3)
    populations_list <- map_depth(populations_list, 2, unique)

  map(populations_list, flatten_chr) %>%
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

# barchart(Titanic, scales = list(x = "free"), auto.key = list(title = "Survived"))
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
#' shared_clonotype <- function(rep_list, clonotype_col) {
#'   # make sure each vector has unique sequences
#'   clonotype_list <- get_clonotypes_list(rep_list, aa_col=clonotype_col, out_type="unique")
#'   # flatten the list of lists into a single list
#'   clonotype <- unlist(clonotype_list)
#'   # generate a frequency table of the clonotype sample collection
#'   clonotype_n <- vec_count(clonotype)
#'
#'   setNames(clonotype_n, c("clonotype", "shared"))
#'
#'   # convert the frequency table into a data frame
#'   # as.data.frame(clonotype_n, responseName = c("sharing"))
#' }















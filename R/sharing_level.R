# number of vectors each sequence presented in
sharing_levle <- function(seq_list1, seq_list2, ...) {

  library("purrr")

  unique_clonotypes <- map(c(seq_list1, seq_list2, ...), unique)

  clonotype_collection <- unlist(unique_clonotypes, use.names = FALSE)

  table(clonotype_collection)
}

# number of vectors each sequence presented in
sharing_levle <- function(seq_list1, seq_list2, ...) {

  library("purrr")

  unique_clonotypes <- map(c(seq_list1, seq_list2, ...), unique)

  clonotype_collection <- unlist(unique_clonotypes, use.names = FALSE)

  table(clonotype_collection)
}

sharing_level_per_gruop <- function(dtl) {
  acast(dtl,
        aaSeqCDR3 ~ group,
        value.var = "sample",
        fun.aggregate = n_distinct)
}

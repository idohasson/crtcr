my_rescale <- function(data, var, factor = 10) {
  data %>% dplyr::mutate("{{ var }}" := {{ var }} / .env$factor)

  # df %>% sample_n(10) %>% sharing_level_per_gruop %>% as.data.frame() %>% my_rescale("A")
}

library(reshape2)

cr_lvl.character <- function(clones) {
  fct_count(factor(clones))
  # vec_count
  # cr_lvl(x$aaSeqCDR3)
}

cr_lvl.list.character <- function(clones) {
  # if (!list_all_vectors(clones)) {
  #   print("error")
  # }
  map_dfr(clones, vec_count, .id = "sample") %>%
    data.table::setnames("key", "aa") %>%
    pivot_wider(names_from = sample, values_from = count, values_fill = 0)

  # df %>% sample_frac(size = .1) %>% split(~ sample) %>% lapply(pull, aaSeqCDR3) %>% cr_lvl.list.character %>% head
}


cr_level.data.frame <- function(df) {
  acast(df,
        aaSeqCDR3 ~ sample,
        value.var = "nSeqCDR3",
        fun.aggregate = n_distinct)

  # df %>% sample_n(10) %>% cr_level.data.frame
}





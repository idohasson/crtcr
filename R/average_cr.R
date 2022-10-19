average_cr.vector <- function(aa) {
  sum(aa) / sum(aa!=0)
}


average_cr.list..vector <- function(aa) {
  sum(aa) / sum(aa!=0)
}

list(l1, l2, l3) %>%
  setNames(LETTERS[1:3]) %>%
  map(flatten_chr) %>%
  map_dfr(vec_count, .id = "group") %>%
  as_tibble()
# %>%
#   pivot_wider(names_from = group, values_from = count, values_fn = ~mean(.x, na.rm = TRUE))

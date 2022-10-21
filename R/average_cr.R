


cr_average  <- function(clonotype, distinct_by) {


  # To calculate the averaged CR of a sample, we measure the CR level for each AA sequence and
  # averaged all the CR levels of the sequences appeared in a specific sample.

  cl <- cr_level(clonotype, distinct_by)

  mean(cl$cr_level)

  # map_dfr(l1, vec_count, .id = "sample") %>%
  #   group_by(sample) %>%
  #   summarise(avg_cr = mean(count))
}


# l1 %>%
#   map(function(aa) {
#     vec_count(aa) %>%
#       pull(count) %>%
#       mean
#   })
#
# lapply(l1, vec_count) %>%
#   map(pull, count) %>%
#   map(mean) %>%
#   as.data.frame()

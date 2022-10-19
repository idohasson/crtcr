library(tidyr)
library(vctrs)
library(reshape2)

# aa <- "aaSeqCDR3"
# clonotype <- map_dfr(dfl, distinct_at, aa, .id = "sample")

# clonotype[,"sample"] <- factor(clonotype$sample,
#                                levels = unlist(gl, use.names = FALSE),
#                                labels = rep(names(lengths(gl)), lengths(gl)))




# as.data.frame(clonotype) %>% vec_count %>% head

# s <- state_count(aa_count, gl) %>%
#   factor(levels = 0:length(gl),
#          labels = c("private", "exclusive", rep("inclusive", length(gl)-1)))

# vec_count(clonotype$aaSeqCDR3) %>% head

# map_dfr(clones, vec_count, .id = "sample")
# aa_count <- map(dfl, pull, aaSeqCDR3) %>%
#   cr_lvl.list.character %>%
#   as.data.frame


# t <- map_dfr(dfl, distinct, aaSeqCDR3, .id = "sample")
# t[,"sample"] <- factor(t$sample, level=(1:6), labels = c(rep("A", 3), rep("B", 3)))
# t <- as.data.frame(t)
# gc <- vec_count(t)

# tapply(as.list())



# tapply(vector, index, function)


# cr_level <- function(df) {
#   acast(df,
#         aaSeqCDR3 ~ sample,
#         value.var = "nSeqCDR3",
#         fun.aggregate = n_distinct)
#
#   # df %>% sample_n(10) %>% cr_level.data.frame
# }

cr_level.list.df <- function(dfl, aa) {
  map_dfr(dfl, distinct_at, aa, .id = "sample") %>%
    vec_count
    # cr_lvl.list.character
    # bind_rows(dfl, .id = "sample") %>%
    # cr_level
}


cr_lvl.character <- function(clones) {

  fct_count(clones)
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

clonotype_table <- function(clones) {
  cr_lvl.list(clones) %>%
    with(table(f, sample))
}






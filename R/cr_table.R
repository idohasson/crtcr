library(vctrs)
library(purrr)
library(dplyr)
library(tidyr)
library(reshape2)
library(data.table)


cr_prop <- function(clone_list, ..., group_list) {
  # tbl <- cr_table(l1, l2, l3)
  cr_table(clone_list, ...) %>%
    reshape2::melt(id = "type") %>%
    xtabs(formula = value ~ variable + type)
  # %>%
  #   proportions("type")
}


cr_table <- function(clone_list, ..., group_list) {

  list(clone_list, ...) %>%
    lapply(map, .f = vec_unique) %>%
    lapply(flatten_chr) %>%
    set_names(LETTERS[seq_along(.)]) %>%
    map_dfr(vec_count, .id = "group") %>%
    pivot_wider(names_from = group,
                values_from = count,
                values_fill = 0) %>%
    tibble::column_to_rownames("key") %>%
    tibble::add_column(type = cr_type(.))
}

cr_type <- function(group_tbl) {

  compute_type <- function(tbl)
  # public clonotype    inclusive clonotype
  # can't be private    multiple shared samples
  (rowSums(tbl) > 1) + (rowSums(tbl != 0) > 1)
  # private = 0 | exclusive = 1 | inclusive = 2

  factor(compute_type(group_tbl),
         levels = c(0, 1, 2),
         labels = c("private", "exclusive", "inclusive"))

}

t <- cr_table(l1, l2) %>%
  tibble::rownames_to_column(var = "aaSeqCDR3")

df1 <- distinct(dfl[[1]], aaSeqCDR3, nSeqCDR3)
right_join(t, df1, by = "aaSeqCDR3")

# l1 <- sapply(rpois(rpois(1, 10), 10), rand_aa)
# l2 <- sapply(rpois(rpois(1, 10), 10), rand_aa)
# l3 <- sapply(rpois(rpois(1, 10), 10), rand_aa)


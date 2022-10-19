library(data.table)
library(dplyr)


public_cr <- function(df_list, group_index) {




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


group_count <- function(group_list) {

  stopifnot(is.list(group_list))

  d <- vec_depth(group_list)

  stopifnot(d > 2)

  if (is.null(names(group_list))) {
    names(group_list) <- seq_along(group_list)
  }

  group_list <- map_depth(group_list, d-1, vec_unique)

  clonotype_count <- map_dfr(group_list, .id = "group", function(group) {
    unlist(group) %>%
      vec_count() %>%
      rename(clonotype = "key")
  })

  pivot_wider(clonotype_count,
              names_from = "group",
              values_from = "count",
              values_fill = 0)

  # clonotype_count
}



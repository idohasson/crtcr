library("tidyselect")
library("tibble")
library("purrr")
library("forcats")
library("stringi")
#
#
# library("dplyr")
library("readr")
paths <- list.files("../../../Datasets/Multiple sampled mice/data/Beta/", full.names = TRUE)[-7]
dfl <- lapply(paths, read_tsv, show_col_types = FALSE)
aa_list <- lapply(dfl, pull, "aaSeqCDR3")
# l1 <- aa_list[1:3]
# l2 <- aa_list[4:6]

rand_aa <- function(vec_size, l=5) {
  do.call(paste0, Map(stri_rand_strings, n=vec_size, length=l, pattern = '[A-C]'))
}



get_group <- function(grouped) {

  # if (is.vector(grouped)) {
  #   return(vec_group_loc(grouped))
  # }

  vec_group_id(grouped)
  # as_tibble(vec_group_loc(grouped))
  # x %>%
  #   as.data.frame %>%





  # n_fields(df) == 2
  # df %>%
  #   as.data.frame %>%
  #   vec_group_loc
}


get_gruops <- function(dfl, gl, ...) {

  if (is.null(names(dfl))) {
    names(dfl) <- LETTERS[seq_along(dfl)]
  }

  if (is.null(names(gl))) {
    names(gl) <- letters[seq_along(gl)]
  }


  get_df <- function(indices) map_dfr(dfl[indices], get_clones.df, ..., .id = "sample")

  map_dfr(gl, get_df, .id = "group")

  # get_gruops(dfl, list(1:2, 3:4, 5:6))
}


# AA_list <- c("A","C","D","E","F","G","H","I","K","L","M","N","P","Q","R","S","T","V","W","Y","*")
# library("dplyr")
# library("readr")
# dfl <- list.files("../../../Datasets/Multiple sampled mice/data/Beta/",
                  # full.names = TRUE)[-7] %>%
  # lapply(read_tsv, show_col_types = FALSE)
#
# get_clonotype(dfl, "aaSeqCDR3")
# unique_clonotypes(dfl, "aaSeqCDR3") %>% lengths
# unique_clonotypes(dfl, "aaSeqCDR3", "nSeqCDR3") %>% lengths


get_clonotype <- function(df_list, clone_col, unique_properties) {
  # df_list <- squash(population)

  stopifnot(is.list(df_list))
  stopifnot(every(df_list, is.data.frame))
  stopifnot(is.character(clone_col))

  if (missing(unique_properties)) {

    unique_properties <- clone_col

  } else {

    stopifnot(is.character(unique_properties))

    unique_properties <- union(clone_col, unique_properties)
  }

  map(df_list, distinct_at, unique_properties, .id = "sample") %>%

    map(pull, clone_col)

    # rename(clonotype = clone_col)

  # map(df_list, distinct_at, unique_properties) %>%

    # map_dfr(rename, clonotype = clone_col, .id = "sample")


}
# x <- get_clonotype(dfl, "aaSeqCDR3") %>%
#   pivot_wider(names_from = sample,
#               values_from = clonotype)
# length(x$`1`)
# x


# get_clonotype <- function(df_list, clone_col) {
#
#   stopifnot(is.list(df_list))
#   stopifnot(every(df_list, is.data.frame))
#
#   stopifnot(is.character(clone_col))
#   stopifnot(length(clone_col) == 1)
#
#   if (is.null(names(df_list))) {
#     names(df_list) <- as.character(seq_along(df_list))
#   }
#
#   map(df_list, distinct_at, clone_col) %>%
#
#     map(vec_unique)
#
# }

# get_clonotype(dfl, "aaSeqCDR3", "nSeqCDR3") %>% head()








get_flat_group <- function(sample_list, aa_col, unique_properties) {

  # TODO: use 'get_clonotype' to get 'clonotype_list'
  # map(sample_list, get_clonotype, aa_col, unique_properties)
  # clonotype_list <- map(sample_list, get_clonotype, "aaSeqCDR3", "nSeqCDR3")

  clonotype_list <- map(sample_list, aa_col)

  flatten_chr(clonotype_list)
}



# get_group <- function(grouped) {
#
#   if (is.vector(grouped)) {
#     return(vec_group_loc(grouped))
#   }
#
#   vec_group_id(grouped)
  # as_tibble(vec_group_loc(grouped))
  # x %>%
  #   as.data.frame %>%

  # n_fields(df) == 2
  # df %>%
  #   as.data.frame %>%
  #   vec_group_loc
# }



<<<<<<< HEAD
# library("tidyselect")
# library("tibble")
# library("purrr")
# library("forcats")
# library("stringi")
# #
# #
# # library("dplyr")
# library("readr")
# paths <- list.files("../../../Datasets/Multiple sampled mice/data/Beta/", full.names = TRUE)[-7]
# dfl <- lapply(paths, read_tsv, show_col_types = FALSE)
# aa_list <- lapply(dfl, pull, "aaSeqCDR3")
# # l1 <- aa_list[1:3]
# # l2 <- aa_list[4:6]
=======
# library("dplyr")
# library("readr")
# dfl <- list.files("../../../Datasets/Multiple sampled mice/data/Beta/", full.names = TRUE)[-7] %>%
#   lapply(read_tsv, show_col_types = FALSE)
#
# get_clonotype(dfl, "aaSeqCDR3")
# unique_clonotypes(dfl, "aaSeqCDR3") %>% lengths
# unique_clonotypes(dfl, "aaSeqCDR3", "nSeqCDR3") %>% lengths
#
# AA_list <- c("A","C","D","E","F","G","H","I","K","L","M","N","P","Q","R","S","T","V","W","Y")
# rand_aa <- function(size) paste(sample(AA_list, size, replace = TRUE), collapse = "")
# reand_sample <- function(n_seq, size) replicate(n_seq, rand_aa(size))
# rand_group <- function(sample_n, seq_n, seq_size) replicate(sample_n, reand_sample(seq_n, seq_size), simplify = FALSE)

# group_list <- list(control = replicate(20, rand_aa(3)),
#                    precancer = replicate(20, rand_aa(3), simplify = FALSE),
#                    cancer = replicate(20, rand_aa(3), simplify = FALSE))


# get_clonotype <- function(df, clone_col, unique_properties) {
#   # df_list <- squash(population)
#
#   stopifnot(is.list(df_list))
#   stopifnot(every(df, is.data.frame))
#   stopifnot(is.character(clone_col))
#
#   if (!missing(unique_properties)) stopifnot(is.character(unique_properties))
#
#   map(df, distinct_at, unique_properties, .id = "sample") %>%
#
#     map(pull, clone_col)
#
#     # rename(clonotype = clone_col)
#
#   # map(df, distinct_at, unique_properties) %>%
#
#     # map_dfr(rename, clonotype = clone_col, .id = "sample")
#
#
# }

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
>>>>>>> 9f74b26107c376199e10ba20871ffb01fa55e3ad
#
# rand_aa <- function(vec_size, l=5) {
#   do.call(paste0, Map(stri_rand_strings, n=vec_size, length=l, pattern = '[A-C]'))
# }
<<<<<<< HEAD
# #
#
#
=======

# get_clonotype(dfl, "aaSeqCDR3", "nSeqCDR3") %>% head()








# get_flat_group <- function(sample_list, aa_col, unique_properties) {
#
#   # TODO: use 'get_clonotype' to get 'clonotype_list'
#   # map(sample_list, get_clonotype, aa_col, unique_properties)
#   # clonotype_list <- map(sample_list, get_clonotype, "aaSeqCDR3", "nSeqCDR3")
#
#   clonotype_list <- map(sample_list, aa_col)
#
#   flatten_chr(clonotype_list)
# }



>>>>>>> 9f74b26107c376199e10ba20871ffb01fa55e3ad
# get_group <- function(grouped) {
#
#   # if (is.vector(grouped)) {
#   #   return(vec_group_loc(grouped))
#   # }
#
#   vec_group_id(grouped)
#   # as_tibble(vec_group_loc(grouped))
#   # x %>%
#   #   as.data.frame %>%
#
#
#
#
#
#   # n_fields(df) == 2
#   # df %>%
#   #   as.data.frame %>%
#   #   vec_group_loc
# }
#
#
# get_gruops <- function(dfl, gl, ...) {
#
#   if (is.null(names(dfl))) {
#     names(dfl) <- LETTERS[seq_along(dfl)]
#   }
#
#   if (is.null(names(gl))) {
#     names(gl) <- letters[seq_along(gl)]
#   }
#
#
#   get_df <- function(indices) map_dfr(dfl[indices], get_clones.df, ..., .id = "sample")
#
#   map_dfr(gl, get_df, .id = "group")
#
#   # get_gruops(dfl, list(1:2, 3:4, 5:6))
# }
#

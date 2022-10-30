#' Title
#'
#' @param nt_vec
#'
#' @return
#' @export
#'
#' @examples
translate <- function(nt_vec) {

  # code <- c('A'= 0, 'C' = 1, 'G' = 2, 'T' = 3)
  # aa_code <- "KQE*TPASRRG*ILVLNHDYTPASSRGCILVFKQE*TPASRRGWMLVLNHDYTPASSRGCILVF"
  code <- c(T = 0, C = 1, A = 2, G = 3)
  # aa_code <- "FFLLSSSSYY**CC*WLLLLPPPPHHQQRRRRIIIMTTTTNNKKSSRRVVVVAAAADDEEGGGG"
  aa_code <- strsplit("FFLLSSSSYY**CC*WLLLLPPPPHHQQRRRRIIIMTTTTNNKKSSRRVVVVAAAADDEEGGGG", "")[[1]]

  nt_to_aa <- function(s) aa_code[code[s[seq(1, length(s) ,3)]] * 16 +
                                  code[s[seq(2, length(s) ,3)]] * 4 +
                                  code[s[seq(3, length(s) ,3)]] + 1]

  strsplit(nt_vec, "") %>%
    lapply(nt_to_aa) %>%
    sapply(paste, collapse="")

}
translate(rep$nSeqCDR3)


# library("dplyr")
# library("readr")
# dfl <- list.files("../../../dataset/clonotypes/Beta/", full.names = TRUE)[-7] %>%
# # dfl <- list.files("../../../Datasets/Multiple sampled mice/data/Beta/", full.names = TRUE)[-7] %>%
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
#
# }

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



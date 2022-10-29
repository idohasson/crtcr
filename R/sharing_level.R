
#' count clonotype-sharing among individuals
#'
#' @description for a given list of amino acid sequences vectors of each individual/sample count number of samples each clonotype is presented
#'
#' @param clonotype_list list of character vectors
#'
#' @return data frame
#' @export
#'
#' @examples
#' # Generate a random list of letters (as the clonotype sequences) from the letters vector.
#' l <- replicate(10, sample(LETTERS, sample(8:15, 1)), simplify = FALSE) # lists of random letters
#' share_level(l)
#'
share_level <- function(clonotype_list) {
  # make sure each vector has unique sequences
  clonotype_list <- lapply(clonotype_list, unique)
  # flatten the list of lists into a single list
  clonotype <- unlist(clonotype_list)
  # generate a frequency table of the clonotypes
  clonotype_n <- table(clonotype)
  # convert the frequency table into a data frame
  as.data.frame(clonotype_n, responseName = c("sharing"))
}


# share_level <- function(group_list) {
#   # 'group_list' is list of character vectors of the samples' clonotype.
#   # 1 clonotypes per sample
#   # unique vector of each sample concatination
#
#   gruop_count <- map_dfr(.x = group_list,
#                          .f = vec_count,
#                          .id = "group")
#
#
#   gruop_count <- pivot_wider(gruop_count,
#                              names_from = "group",
#                              values_from = "count",
#                              values_fill = 0) %>%
#     column_to_rownames("key")
#
#   rowSums(gruop_count > 1)
#
# }



# x <- unique_clonotypes(dfl, "aaSeqCDR3", "nSeqCDR3") %>%


# x <- as.list(1:3) %>%
#   lapply(pluck, .x = dfl) %>%
#   get_clonotype("aaSeqCDR3") %>%
#   share_level


# rowSums(x != 0)
#
# x[1:10,] %>%    pivot_wider(
#                                           names_from = "sample",
#                                           values_from = "clonotype",
#                                           values_fill = 0)
#
#
# list(g1 = dfl[c(1:3)], g2 = dfl[c(4:6)]) %>%
#   lapply(get_clonotype, "aaSeqCDR3") %>%
#   map_dfr(share_level, .id = "group")




  # map_dfr(vec_count, .id = "group") %>%
  #


share_table <- function(clone_list) {

  # population <- list(clone_list, ...)

  # if (vec_depth(population) > 2) {
  #
  #   population <- modify_depth(aa_l, 2, unlist)
  #
  #   }

  lapply(clone_list, vec_unique) %>%

    map_dfr(vec_count, .id = "group") %>%

    pivot_wider(names_from = group,
                values_from = count,
                values_fill = 0) %>%

    rename(clonotype = "key")
}


# # share_prop <- function(clone_list, ...) {
# #   # list(clone_list, ...)
# #   cr_table(clone_list, ...) %>%
# #     reshape2::melt(id = "type") %>%
# #     xtabs(formula = value ~ variable + type)
# #   # %>%
# #   #   proportions("type")
# # }
#
# # number of vectors each sequence presented in
#
# sharing_levle <- function(seq_list, ...) {
#
#   unique_clonotypes <- map(c(seq_list, ...), unique)
#
#   clonotype_collection <- unlist(unique_clonotypes, use.names = FALSE)
#
#   map(clonotype_collection, )
#
# }
#
# sharing_level_per_gruop <- function(dtl) {
#
#   acast(dtl,
#         aaSeqCDR3 ~ group,
#         value.var = "sample",
#         fun.aggregate = n_distinct)
#
# }

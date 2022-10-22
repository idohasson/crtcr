
cr_level <- function(df_list, clone_col, match_var) {

  unique_clonotypes(df_list, clone_col, match_var) %>%

    map_dfr(vec_count, .id = "sample") %>%

    pivot_wider(names_from = sample,
                values_from = count,
                values_fill = 0)
}

cr_level <- function(clonotype, distinct_by) {


  # Here, we define CR level as the number of different NT sequences encoding a specific AA
  # sequence. To calculate the averaged CR of a sample, we measure the CR level for each AA sequence and
  # averaged all the CR levels of the sequences appeared in a specific sample.

  # immuneREF feature: Repertoire overlap (convergence)
  # The pairwise repertoire clonal overlap (clones defined based on 100%
  # similarity of CDR3 amino acid sequence), was calculated across repertoires,
  # as previously described

  stopifnot("clonotype must be a character vector"= is.character(clonotype))

  if (!missing(distinct_by)) {
    stopifnot("'distinct_by' must be a character vector"= is.character(clonotype))
    stopifnot('distinct_by must be with equal lengths'= length(clonotype) != length(distinct_by))
    # TODO
  }

  cc <- vctrs::vec_count(clonotype, "none")
  cc <- setNames(cc, c("clonotype", "cr_level"))
  cc
}

# cr_level.character <- function(v) {
#   cc <- vctrs::vec_count(v, "none")
#   setNames(cc, c("clonotype", "cr_level"))
# }
#
# cr_level.data.frame <- function(v) {
#   cc <- vctrs::vec_count(v, "none")
#   setNames(cc, c("clonotype", "cr_level"))
# }





# cr_level <- function(df, clones, unique_by) {
#
#   df <- distinct_at(df, c(clones, unique_by))
#
#   vec_count(pull(df, clones)) %>%
#
#     rename(clonotype = "key")
#
#   # cr_level(BRCA1, "aaSeqCDR3", "nSeqCDR3")
# }
#
#
#
# # cr_level.list  <- function(population) {
# #
# #   stopifnot(is.list(population))
# #
# #   if (vec_depth(population) > 2) {
# #     population <- modify_depth(aa_l, 2, unlist)
# #   }
# #
# #   lapply(population, vec_unique) %>%
# #
# #     map_dfr(vec_count, .id = "sample") %>%
# #
# #     pivot_wider(names_from = sample,
# #                 values_from = count,
# #                 values_fill = 0)
#
#
#
#   # if (d > 1) {
#   #
#   # }
#   #
#   # map(population, )
#
#   # stopifnot(is.vector(seq_list))
#
#   # if (missing(sample_name)) {
#   #   sample_name = LETTERS[seq_along(seq_list)]
#   # }
#
#
#
#   # seq_list %>%
#   #
#   #   map_dfr(vec_count, .id = "sample") %>%
#   #
#
#   #
#   #   tibble::column_to_rownames("key")
# # }
#
#
#
#
#
# #
# # groups <- mtcars[c("vs", "am")]
# # vec_group_id(groups)
# # field(group_rle, "group")
# # field(group_rle, "length")
#
# # map_dfr(l1, vec_count, .id = "sample")
# # cr[c("sample", "key")] %>%
# #   as.list() %>%
# #   tapply(X = cr$count , FUN = sum)
# # cr[c("key", "sample")] %>%
# #   tapply(X = cr$count, FUN = quantile)
# # tapply(warpbreaks$breaks, warpbreaks[,-1], sum)
#
#
#
# # cr_level <- function(df_list, by_value, measure_by) {
# #   # cr_level.data.frame
# #   df <- bind_rows(df_list, .id = "sample")
# #
# #   clonotypes <- melt(data = df,
# #                      id.vars=c("sample", clone_col),
# #                      measure.vars = cr_by, na.rm=TRUE)
# #
# #   cr <- acast(clonotypes, aaSeqCDR3 ~ sample, n_distinct)
# #
# #   acast(df,
# #         aaSeqCDR3 ~ sample,
# #         value.var = by_value,
# #         fun.aggregate = n_distinct)
# # }
#
# # tapply(vector, index, function)
#
#
#
#
#
#
#
# # map(dfl, pull, aaSeqCDR3) %>%
# #   cr_level()
#
#
#
#
#
#
#
#

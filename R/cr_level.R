#' Convergent recombination level calculation
#'
#' @param ... character vector(s) of the clonal sequences.
#' @param ignore.na if TRUE (default), NA values are not included in the count.
#'
#' @return integer of unique count.
#' @export
#'
#' @examples
#'
#' cr_level(rep(LETTERS[1:2], each=4), gl(2,2,8), gl(4,3,8))
#'
cr_level <- function(clones, ignore.na=TRUE) {

  # clone_lists <- list2(...)
  #
  # clonal_sequences <- squash_chr(clone_lists)

  if (isTRUE(ignore.na)) {

    no_na <- vec_detect_complete(clones)

    clonal_sequences <- clones[no_na]

  }

  vec_unique_count(clones)

}

subgroup_cr_level <- function(.clone, .clonotype, .id, .subgroup, ...) {

  tbl <- tapply(.clone, rlang::list2(.clonotype, .subgroup, .id, ...), cr_level)

  apply(tbl, 1:2, mean, na.rm = T)

}





# with(rand_df, subgroup_cr_level(clone, clonotype, rep_id, group))

# tapply(rand_df$clone, as.list(rand_df[-3]), cr_level) %>%
#   apply(c("clonotype", "group"), mean, na.rm = T)



# cr_level_df <- function(.data, .clonal_seq, .clonotype_seq) {
#
#
#
#
#   if (missing(.clonotype_seq)) {
#
#     group_by(.data, clonotype=translate({{.clonal_seq}}), .add = TRUE) %>%
#
#       summarise(CR_level = cr_level({{.clonal_seq}}))
#
#   } else {
#
#     group_by(.data, {{.clonotype_seq}}, .add = TRUE) %>%
#
#       summarise(CR_level = cr_level({{.clonal_seq}}))
#
#   }
#
# }

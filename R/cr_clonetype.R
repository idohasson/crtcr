

#' Title
#'
#' @param v1 character vector of the first populations
#' @param v2 character vector of the second populations
#' @param ... any additional character vectors
#'
#' @return list of private, inclusive and exclusive clonotype
#' @export
#'
#' @examples
#'
#'
#'
cr_clonotype <- function(v1, v2, ...) {

  l <- list(v1, v2, ...)

  names(l) <- as.character(seq_along(l))

  clonotype_count <- l %>%

    map_dfr(vctrs::vec_count, .id = "group") %>%

    spread(group, count, fill = 0)

  cr <- cr_type(clonotype_count[-1])

  split(clonotype_count$key, cr)

}


cr_clonotype_list <- function(l1, l2, ...) {

  l <- list(l1, l2, ...) %>%

    set_names(paste("l", seq_along(.), sep = "")) %>%

    modify_depth(2, vec_unique)


  clonotype_count <- simplify_all(l, .type = "character") %>%

    map_dfr(vctrs::vec_count, .id = "group") %>%

    spread(group, count, fill = 0)


  cr_type(clonotype_count[-1]) %>%

    split(x = clonotype_count$key)






  # mapply(FUN = function(lv) flatten_chr(map(lv, unique)))
  #
  #
  # clonotype_count <- l %>%
  #
  #   map_dfr(vctrs::vec_count, .id = "group") %>%
  #
  #   spread(group, count, fill = 0)
  #
  #
  # clonotype_count



  # cr_type(clonotype_count[-1]) %>%
  #
  #   split(x = clonotype_count$key, cr)

    # mapply(FUN = function(l) flatten_chr(map(l, unique))) %>%
    #
    # cr_clonotype

  # lapply(g1, unique) %>%
  #
  #   simplify(.type = "character")

}


#' Title
#'
#' @param count_table column is the number of samples having a specific clonotype in a row
#'
#' @return list of clonotype in each sub population
#' @export
#'
#' @examples
cr_type <- function(count_table) {

  not_singletone <- apply(count_table, 1, function(n) sum(n != 0) > 1)

  not_single <- rowSums(count_table) > 1

  dplyr::case_when(
    not_singletone ~ "inclusive",
    not_single ~ "exclusive",
    TRUE ~ "private")
}

#
# cr_type <- function(group_tbl) {
#
#   compute_type <- function(tbl)
#     # public clonotype    inclusive clonotype
#     # can't be private    multiple shared samples
#     (rowSums(tbl) > 1) + (rowSums(tbl != 0) > 1)
#   # private = 0 | exclusive = 1 | inclusive = 2
#   factor(compute_type(group_tbl),
#          levels = c(0, 1, 2),
#          labels = c("private", "exclusive", "inclusive"))
# }



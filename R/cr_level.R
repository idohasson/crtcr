

#' CR level table
#'
#' @param ... repertoire list / data frames vectors
#'
#' @return table
#'
#' @importFrom dplyr distinct_at
#' @importFrom magrittr %<>% %$%
#'
#' @export
#'
#' @examples
#'
#' g1 <- group_gen()
#' g2 <- group_gen()
#' g3 <- group_gen()
#' g4 <- group_gen()
#'
#' # generate random
#' g <- list(g1, g2, g3, g4)
#'
#' g <- lapply(seq_along(g), function(i) g[[i]] %>%
#'      setNames(paste0(LETTERS[i], seq_along(.))))
#'
#' cr_level(g)
#'
#'
#'
cr_level <- function(...) { # TODO: allow data frame input of 'group_join' output

  df <- group_join(...)

  df %<>% distinct(group, rep_id, clone, .keep_all = TRUE)

  df %$% table(clonotype, rep_id, group) %>%

  ftable(row.vars="clonotype", col.vars="rep_id") %>% as.table()

}


group_by_at(df, c("group","rep_id","clonotype")) %>%

  summarise_at(clone_col, n_distinct)



cr_level_clonotype <- function(rep,  of, clonotype_col, clone_col) {

  df %>% dplyr::group_by_at(c("group","rep_id","clonotype")) %>%

    dplyr::summarise_at(.vars = list("clone"), dplyr::n_distinct) %>%

    as.data.frame()
}











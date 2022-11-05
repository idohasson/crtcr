#################### share-level table ####################
# clonotype sharing between individuals #

# Previous studies have shown that the extent of sharing and the
# clonotypic frequency of TCRb sequences are significantly corre
# lated with their production efficiencies in simulations of a random
# recombination process because of the phenomenon of convergent
# recombination. https://doi.org/10.1073/pnas.1319389111

#' Title
#'
#' @param ... data frames or vectors
#' @param by x
#'
#' @return table
#' @export
#'
#' @examples
#'
#' nt_gen <- clone_gen()
#'
#' group1 <- list(sapply(rpois(100, 2)+1, nt_gen), sapply(rpois(100, 2)+1, nt_gen))
#' group2 <- list(sapply(rpois(100, 2)+1, nt_gen), sapply(rpois(100, 2)+1, nt_gen))
#'
#' group_join(list(a=group1, b=group2))#'
#'
sharing <- function(..., by=c("clonotype", "rep_id")) { # DF
  group_join(...) %>%
  select_at(by) %>%
  distinct() %>%
  table()
}

nt_gen <- clone_gen()

group1 <- list(sapply(rpois(100, 2)+1, nt_gen), sapply(rpois(100, 2)+1, nt_gen))
group2 <- list(sapply(rpois(100, 2)+1, nt_gen), sapply(rpois(100, 2)+1, nt_gen))

x <- group_join(list(a=group1, b=group2)) %>%
  dplyr::distinct_at(c("clonotype", "rep_id")) %>%
  table %>% t



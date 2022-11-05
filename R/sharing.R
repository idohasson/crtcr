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
#'
#' @importFrom dplyr distinct_at
#'
#' @export
#'
#' @examples
#'
#' nt_gen <- clone_gen()
#'
#' group1 <- list(sapply(rpois(, 5)+1, nt_gen), sapply(rpois(,5)+1, nt_gen))
#' group2 <- list(sapply(rpois(100, 7)+1, nt_gen), sapply(rpois(100, 8)+1, nt_gen))
#' group3 <- list(sapply(rpois(100, 2)+1, nt_gen), sapply(rpois(100, 2)+1, nt_gen))
#' group4 <- list(sapply(rpois(100, 2)+1, nt_gen), sapply(rpois(100, 2)+1, nt_gen))
#' group5 <- list(sapply(rpois(100, 2)+1, nt_gen), sapply(rpois(100, 2)+1, nt_gen))
#'
#' groups <- group_join(list(a=group1, b=group2, c=group3, d=group4, e=group5))
#'
sharing <- function(...) { # DF
  # a vector of integers giving the numbers of the variables, or a character vector giving the names of the variables to be used for the columns of the flat contingency table.
  group_join(...) %>%

  distinct(clonotype, group) %$%

  table(clonotype, group)
}


#
# group1 <- list(sapply(rpois(100, 2)+1, nt_gen), sapply(rpois(100, 2)+1, nt_gen))
# group2 <- list(sapply(rpois(100, 2)+1, nt_gen), sapply(rpois(100, 2)+1, nt_gen))
#
# x <- group_join(list(a=group1, b=group2)) %>%
#   dplyr::distinct_at(c("clonotype", "rep_id")) %>%
#   table %>% t

# cr level



# distinct(df)

# distinct_at(df, c("rep_id", "clone", "clonotype"))


# tbl <- table(df[-3])

# sharing(g1, g2, g3, g4) %>% head







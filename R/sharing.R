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
#'
#' @return table
#'
#' @importFrom dplyr distinct
#' @importFrom magrittr %>% %$%
#'
#' @export
#'
#' @examples
#'
#' # generate random
#' g <- replicate(4, rand_group(), simplify = FALSE)
#' sharing(g)
#'
sharing <- function(...) { # DF
  # a vector of integers giving the numbers of the variables, or a character vector giving the names of the variables to be used for the columns of the flat contingency table.
  group_join(...) %>%

  distinct(clonotype, group) %$%

  table(clonotype, group)

}










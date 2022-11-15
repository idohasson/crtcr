#################### Overlaps ####################
cr_overlap <- function(subpopulations, func=intersect_percentage) { # TODO: overlap as number of intersect. not precentile

  intersect_percentage <- function(x, y) { # overlap coefficient
    length(intersect(x, y)) / min(length(x), length(y))
  }

  jaccard_index <- function(x, y){
    length(intersect(x, y)) / length(union(x, y))
  }

  purrr::cross(cr_list) %>%

    lapply(setNames, names(formals(func))) %>%

    purrr::invoke_map(.f = func) %>%

    array(dim = c(3,3), dimnames = lapply(cr_list, names))
}

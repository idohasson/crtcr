#' Compute levels of a numeric vector based on proportions and quantiles
#'
#' This function computes the levels of a numeric vector based on the proportions of values in the vector and a set of quantiles provided as input. It uses the `table`, `proportions`, and `cumsum` functions to calculate the proportions of values in the vector and the corresponding cumulative proportions, and then uses the `vapply` function to apply a function to the provided quantiles to find the first levels that are higher than the quantiles. Finally, it returns the resulting levels, optionally removing any duplicates.
#'
#' @param values a numeric vector
#' @param top a numeric vector of quantiles to use when computing the levels
#' @param rm.dup a logical value indicating whether duplicate levels should be removed
#' @return a numeric vector with the computed levels
#' @examples
#' # Compute the levels for the numbers 1, 2, 3, 4, and 5 using the quantiles 0.25, 0.5, and 0.75
#' top_prec(c(1, 2, 3, 4, 5), c(0.25, 0.5, 0.75))
#'
#' # Compute the levels for the numbers 1, 2, 3, 4, and 5 using the quantiles 0.25, 0.5, and 0.75, and remove duplicates
#' top_prec(c(1, 2, 3, 4, 5), c(0.25, 0.5, 0.75), rm.dup = TRUE)
#' @export
quantile_levels <- function(values, top=c(.25, .5, .75), rm.lvl=TRUE) {

  levels <- quantile(values, top)

  if (isTRUE(rm.lvl)) {

    levels <- levels[!duplicated(levels)]

  }

  return(levels)

}


rank_level <- function(x) rank(x)

#' Compute the level of a numeric vector
#'
#' This function computes the level of each element in a numeric vector by using the provided intervals. The intervals can be specified directly as a numeric vector or as a function that returns a numeric vector. If no intervals are provided, the default is to use the median of the values as the interval.
#'
#' @param values a numeric vector
#' @param intervals a numeric vector or a function that returns a numeric vector
#' @return a numeric vector with the levels for each element in values
#' @examples
#' # Compute the levels for the numbers 1, 2, 3, 4, and 5
#' level(c(1, 2, 3, 4, 5))
#'
#' # Compute the levels for the numbers 1, 2, 3, 4, and 5 using the median as the interval
#' level(c(1, 2, 3, 4, 5), median)
#'
#' # Compute the levels for the numbers 1, 2, 3, 4, and 5 using the function mean as the interval
#' level(c(1, 2, 3, 4, 5), mean)
#' @export
level <- function(values, intervals=median) {

  if (is_formula(intervals))

    intervals <- as_function(intervals)

  if (is_function(intervals))

    intervals <- intervals(values)

  findInterval(values, intervals, rightmost.closed = TRUE)

}

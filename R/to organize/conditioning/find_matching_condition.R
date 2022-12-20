#' @title Find Matching Condition
#'
#' @description This function finds the highest threshold level that satisfies a given condition for a list of numeric vectors. The threshold level and condition can be provided as arguments, or they can be generated from the data. The conditions are applied in the following order: >, >=, ==, <=, <.
#'
#' @param vectors a list of numeric vectors
#' @param threshold_levels a vector of threshold levels for the condition (one is needed for every vector)
#' @param conditions a vector of conditions to apply to the numeric vectors (one is needed for every vector)
#'
#' @return a vector of matching threshold levels
#'
#' @examples
#' # Create a list of numeric vectors
#' vectors <- list(c(1, 2, 3, 4), c(5, 6, 7, 8), c(9, 10, 11, 12))
#'
#' # Find the highest threshold level that satisfies the condition for each vector
#' find_matching_condition(vectors, threshold_levels = c(2, 3, 4), conditions = c(">", ">=", "=="))
#'
#' # Generate threshold levels and conditions from the data
#' find_matching_condition(vectors)
#'
#' @export
#'
find_matching_condition <- function(vectors, threshold_levels=1, conditions=c(">", ">=", "==", "<=", "<")) {
  # Create a data frame from the list of numeric vectors
  value_df <- list2DF(vectors)

  # Generate threshold levels if they are not provided
  if (!is.data.frame(threshold_levels)) {
    threshold_list  <- rlangsplice(threshold_levels)
    var_names <- names(value_df)
    threshold_list  <- vctrs::rep_named(var_names, vctrs::vec_recycle_common(threshold_list ))
    threshold_levels <- list2DF(threshold_list )
  }

  # Generate conditions if they are not provided
  conditions <- rlang::rep_along(value_df, conditions)

  # Find the highest threshold level that satisfies the condition for each vector
  matches <- vctrs::vec_locate_matches(
    needles = value_df, haystack = threshold_levels, condition = conditions,
    multiple = "last", incomplete = NA,
    needles_arg = "*_values", haystack_arg = "*_bound"
  )

  matches$haystack
}

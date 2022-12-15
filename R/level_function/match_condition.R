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


# is_super_public <-  function(cr_count) {
#   super_min_freq <- ~ top_prec(.x, top = .5)
#   level_values <- level(cr_count, super_min_freq)
#   condition(cr=level_values, .condition = cr != 0)
# }
# is_super_public(x)
# condition(x, y, .condition = x>=2 & y<4)
condition <- function(..., .condition) {

  condition_call <- substitute(.condition)

  dfl <- df_list(..., .name_repair = "minimal")

  val <- new_data_frame(dfl)

  i_cond <- eval(condition_call, val)

  i_cond

}



#' Check if values in a data frame match a given condition and bound.
#'
#' @param df_list A list of data frames containing the values to compare.
#' @param bound_level A data frame or list of values representing the bound to compare against.
#' @param condition A character vector of conditions to compare the values against the bound.
#'   Possible values are: ">", ">=", "==", "<=", "<".
#' @return A data frame containing the matching values from the input data frames.
#' @examples
#' df1 <- data.frame(x = c(1, 2, 3), y = c(4, 5, 6))
#' df2 <- data.frame(x = c(10, 20, 30), y = c(40, 50, 60))
#' match_condition(df1, df2, bound_level = 5, condition = ">=")
#'   #>   x  y
#'   #> 1 1  5
#'   #> 2 2  6
#'
# match_condition <- function(df_list, bound_level = 1, condition = c(">", ">=", "==", "<=", "<")) {
#   # convert the df_list argument into a data frame
#   value_df <- new_data_frame(df_list, .name_repair = "unique_quiet")
#
#   # if bound_level is not a data frame, convert it into one
#   if (!is.data.frame(bound_level)) {
#     bound_list <- splice(bound_level)
#     var_names <- names(value_df)
#     bound_list <- rep_named(var_names, vec_recycle_common(bound_list))
#     bound_level <- list2DF(bound_list)
#   }
#
#   # repeat the condition vector to match the number of columns in value_df
#   condition <- rep_along(value_df, condition)
#
#   # locate values in value_df that match the bound and condition
#   matches <- vec_locate_matches(
#     needles = value_df, haystack = bound_level, condition = condition,
#     multiple = "last", incomplete = NA,
#
#


# match_condition <- function(numerical_vectors, ..., bound_level=1,
#                             condition=c(">", ">=", "==", "<=", "<")) {
#
#   dfl <- df_list(..., .name_repair = "unique_quiet")
#   value_df <- new_data_frame(dfl)
#
#   # vector_list <- rlang::dots_splice(numerical_vectors, ...)
#   # value_df <- list2DF(vector_list)
#
#   # Check valid bound
#   if (!is.data.frame(bound_level)) {
#
#     bound_list <- rlangsplice(bound_level)
#
#     var_names <- names(value_df)
#
#     bound_list <- vctrs::rep_named(var_names, vctrs::vec_recycle_common(bound_list))
#
#     bound_level <- list2DF(bound_list)
#
#   }
#
#   condition <- rlang::rep_along(value_df, condition)
#
#   # condition
#   matches <- vctrs::vec_locate_matches(
#
#     needles = value_df, haystack = bound_level, condition = condition,
#
#     multiple = "last", incomplete = NA,
#
#     needles_arg = "*_values", haystack_arg = "*_bound"
#
#   )
#
#   matches$haystack
#
# }

# share_level <- function(aa, id) {
#   vapply(vec_split(id, aa)$val, vec_unique_count, numeric(1))
# }
#
# public_level <- function(aa, id1, id2,
#                          share_val1=share_level(aa, id1),
#                          share_val2=share_level(aa, id2),
#                          bound=list(2,1), condition=c(">=", "<=")) {
#
#   match_condition(public=share_val1,
#                   exclusive=share_val2,
#                   bound_level = bound,
#                   condition = condition)
#
# }
# AA=translate(rand_nt_vec(1000, 2))
# ID1=rep_along(AA, letters[1:16])
# ID2=rep_along(AA, LETTERS[1:3])
# unique(AA[public_level(AA, ID1, ID2)==1])

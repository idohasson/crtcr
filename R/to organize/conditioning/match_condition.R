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
match_condition <- function(numerical_vectors, ..., bound_level=1,
                            condition=c(">", ">=", "==", "<=", "<")) {

  dfl <- df_list(..., .name_repair = "unique_quiet")
  value_df <- new_data_frame(dfl)

  # vector_list <- rlang::dots_splice(numerical_vectors, ...)
  # value_df <- list2DF(vector_list)

  # Check valid bound
  if (!is.data.frame(bound_level)) {

    bound_list <- rlangsplice(bound_level)

    var_names <- names(value_df)

    bound_list <- vctrs::rep_named(var_names, vctrs::vec_recycle_common(bound_list))

    bound_level <- list2DF(bound_list)

  }

  condition <- rlang::rep_along(value_df, condition)

  # condition
  matches <- vctrs::vec_locate_matches(

    needles = value_df, haystack = bound_level, condition = condition,

    multiple = "last", incomplete = NA,

    needles_arg = "*_values", haystack_arg = "*_bound"

  )

  matches$haystack

}

# AA=translate(rand_nt_vec(1000, 2))
# ID1=rep_along(AA, letters[1:16])
# ID2=rep_along(AA, LETTERS[1:3])
# unique(AA[public_level(AA, ID1, ID2)==1])

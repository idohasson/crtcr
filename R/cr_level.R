#' Convergent recombination level calculation
#'
#' @param .clone character vector of the clonal sequences.
#' @param ... Additional vectors of clonal features to distinct same clonal sequences optional
#'
#' @return integer of unique count.
#' @export
#'
#' @examples
#'
#' cr_level(rep(LETTERS[1:2], each=4), gl(2,2,8), gl(4,3,8))
#'
cr_level <- function(.clone, ...) {

  unique_n(.clone, ...)

  # apply_unique <- function(.on, ..., .apply_func) {
  #
  #   .by <- df_list(..., .name_repair = "minimal")
  #
  #   .by <- new_data_frame(.by)
  #
  #   .by <- vec_group_id(.by)
  #
  #   tapply(.on, .by, .apply_func)
  #
  # }

}












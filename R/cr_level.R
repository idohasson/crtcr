#' Convergent recombination level calculation.
#'
#' unique number of values / rows.
#'
#' @param clonal_seq character vector
#' @param na.rm
#' @param ... Data frame or vectors of equal-length.
#'
#' @return integer
#' @export
#'
#' @examples
#'
#' cr_level(c("A", "B","A", "B", "B", NA))
#'
#' cr_level(c("s", "s", "a", "a"), c(1:3,NA), na.rm = FALSE)
#'
cr_level <- function(clonal_seq,..., na.rm=FALSE) {

  clonal_seq

  clone_data <- df_list(..., .name_repair = "minimal")

  clone_data <- new_data_frame(clone_data)

  if (isTRUE(na.rm)) {

    all_non_missing <- vec_detect_complete(clone_data)

    clone_data <- vec_slice(clone_data, all_non_missing)

  }

  vec_unique_count(clone_data)
}

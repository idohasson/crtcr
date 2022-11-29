#' Number of distinct repertoires
#'
#' @param .id vector of repertoire's identifier
#' @param ... additional identifiers to distinguish between same `.id` value.
#'
#' @return integer of the repertoire count.
#' @export
#'
#' @examples
#'
#' v1 <- c("A", "A", "A", "B", "A", "B")
#' sharing_level(v1)
#'
#' (v2 <- rep(1:2, each=3))
#' sharing_level(v1, v2)
#'
#' (v3 <- rep(1:2, times=3))
#' sharing_level(v1, v2, v3)
#'
sharing_level <- function(.id, ...) {

  .data <- new_data_frame(df_list(.id, ..., .name_repair = "minimal"))

  vctrs::vec_count(.data)

}



# sharing_subgroup <- function(.id, .subgroup, ...) {
#
#   .data <- new_data_frame(df_list(.subgroup, ..., .name_repair = "minimal"))
#
#   tapply(.id, .subgroup, unique_n)
#
# }

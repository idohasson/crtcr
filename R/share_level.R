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
sharing_level <- function(.id, share_func=unique_n) {

  rep_i <- .id[vec_detect_complete(.id)]

  vec_unique_count(rep_i)

}



group_sharing <- function(.id, .group, reduce_by=mean, ...) {

  l_i <- split(.id, .group)

  N_v <- list_sizes(l_i)

  reduce_by(N_v, ...)


  # .data <- new_data_frame(df_list(.subgroup, ..., .name_repair = "minimal"))
  #
  # sub_i <- vctrs::vec_group_id(.data)
  #
  # tapply(.id, sub_i, vec_unique_count)
  # sub_id <- vctrs::vec_split(.id, .data)
  # sub_id
  # sapply(sub_id$val, unique_n)

}

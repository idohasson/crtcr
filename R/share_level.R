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



share_number <- function(.id) {
  vec_unique_count(.id)
}

share_mean <- function(.id) {

  share_number(.id) / vec_size(.id)

}

share_level <- function(.id, ..., .share_func=share_number) {

  dfl <- df_list(..., .name_repair = "minimal")

  if (vec_size(dfl) == 0)

    return(.share_func(.id))



  tapply(.id, dfl, .share_func)

}

share_cr <- function(.clone, .id, ...,
                     .cr_func=cr_level,
                     .share_func=share_level) {

  share_level(.id , .clone, .share_func = share_mean) %*% (table(.clone, .id) %*% cr_level(.clone, .id, .cr_func = cr_mean))

  # table(.clone, .id, ...)



  # v1 <- tapply(.clone, .id, .cr_func, ...)
  #
  # v2 <- tapply(.id, .clone, .share_func, ...)
  #
  # v1 %*% v2

}

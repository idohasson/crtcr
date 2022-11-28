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
#' sharing_number(v1)
#'
#' (v2 <- rep(1:2, each=3))
#' sharing_number(v1, v2)
#'
#' (v3 <- rep(1:2, times=3))
#' sharing_number(v1, v2, v3)
#'
sharing_number <- function(.id, ...) {

  unique_n(.id, ...)

}

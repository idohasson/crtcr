#' Is public clonotype
#'
#' @param .id repertoire identifier vector
#' @param ... additional identifier vectors
#' @param .public minimum number of repertoires.
#'
#' @return TRUE for public, FALSE for private.
#' @export
#'
#' @examples
#'
#' is_public(c("A", "A", "A", "A"))
#' is_public(c("A", "A", "A", "A", "B"))
#'
is_public <- function(.id, ..., .public=2) {
  unique_n(.id, ...) >= .public
}

#' Is exclusive public clonotype
#'
#' True for if clonotype’s present across subgroups doesn't surpass the threshold
#'
#' @param .subgroup sub-group identification vector.
#' @param ... additional identifier vectors
#' @param .exclusive maximal number of subgroups a clone is found in.
#'
#' @return TRUE for exclusive, FALSE for inclusive.
#' @export
#'
#' @examples
#'
#' is_exclusive(c("A", "A", "A", "A"))
#' is_exclusive(c("A", "A", "A", "A", "B"))
#'
is_exclusive <- function(.subgroup, ..., .exclusive=1) {
  # TODO: add "is puclic" checking
  unique_n(.subgroup, ...) <= .exclusive

}

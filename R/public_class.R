

#' Public classification
#'
#' returns the publicness classifications "private," "exclusive,” and
#' “inclusive” by the repertoires’ presence, throughout all subgroups.
#'
#' @param .id repertoire identifier vector
#' @param .subgroup sub-group identification vector.
#' @param ... additional identifier vectors.
#'
#' @return "private" / "exclusive” / “inclusive”
#' @export
#'
#' @examples
#'
#' public_class(c("A", "A"), c("C", "D"))
#' public_class(c("A", "B"), c("C", "C"))
#' public_class(c("A", "A"), c("C", "C"))
#'
public_class <- function(.id, .subgroup, ...) {

  PUBLIC_INDEX <- c("private", "inclusive", "exclusive")

  is_p <- is_public(.id, ...)

  is_e <- is_exclusive(.subgroup, ...)

  i <- (is_p) + (is_p & is_e)

  PUBLIC_INDEX[i+1]

}
